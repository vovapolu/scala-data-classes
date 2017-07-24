package fommil.stalagmite.stats

import fommil.stalagmite.DataInfo
import fommil.stalagmite.DataStats

import scala.collection.immutable.Seq
import scala.meta._

object CaseClassStats {

  object DataApplyStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val params = dataInfo.classParams.zip(dataInfo.classParamsTypes).map {
        case (param, tpe) =>
          param.default match {
            case Some(defaultVal) =>
              param"""${Term.Name(param.name.value)}: $tpe = $defaultVal"""
            case None =>
              param"${Term.Name(param.name.value)}: $tpe"
          }
      }

      val (initStats, ctorArgs, finishStats) =
        if (dataInfo.getMod("memoise")) {
          MemoisationStats.specialApplyPart(dataInfo)
        } else {
          (Seq(),
           dataInfo.classParamNames,
           Seq(q"created.synchronized(created)"))
        }

      val (packStats, packedArgs) = if (dataInfo.requiredToPack) {
        HeapOptimizationStats.specialApplyPart(dataInfo, ctorArgs)
      } else {
        (Seq(), ctorArgs)
      }

      Seq(
        q"""def apply[..${dataInfo.simpleTypeParams}](
            ..$params
          ): ${dataInfo.dataType} = {
          ..$initStats
          ..$packStats
          ..${Seq(q"""val created =
               new ${Ctor.Ref.Name(dataInfo.name.value)}(..$packedArgs)""")}
          ..$finishStats
        }"""
        // probably a bug in the parser:
        // single expression between list expansion (..$list) can't be parsed
      )
    }
  }

  object DataUnapplyStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val clFields = dataInfo.classParams.map(
        param => q"that.${Term.Name(param.name.value)}"
      )

      val tupleTypes = dataInfo.classParamsTypes match {
        case Seq(tpe1) => tpe1
        case tpes      => t"(..$tpes)"
      }
      val tupleArgs = clFields match {
        case Seq(field1) => field1
        case fields      => q"(..$fields)"
      }
      Seq(
        q"""def unapply[..${dataInfo.simpleTypeParams}](
            that: ${dataInfo.dataType}): Option[$tupleTypes] =
         Some($tupleArgs)"""
      )
    }
  }

  object DataGettersStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          q"def $param: $tpe = this.${Term.Name("_" + param.value)}"
      }
  }

  object DataEqualsStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val eqs = dataInfo.classParamNames.map(
        paramName => q"that.$paramName == this.$paramName"
      )
      val eqsWithAnds = eqs match {
        case Seq(eq1) =>
          eq1
        case Seq(eq1, rest @ _*) =>
          rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
      }
      val thatEqual = if (dataInfo.getMod("memoiseHashCode")) {
        q"(this eq that) || (this.hashCode == that.hashCode && $eqsWithAnds)"
      } else {
        q"(this eq that) || ($eqsWithAnds)"
      }

      if (dataInfo.getMod("memoiseStrong")) {
        Seq()
      } else {
        Seq(
          q"""override def equals(thatAny: Any): Boolean = thatAny match {
            case that: ${dataInfo.dataPatType} => $thatEqual
            case _ => false
         }"""
        )
      }
    }
  }

  object DataProductMethodsStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val casesForElements = dataInfo.classParamNames.zipWithIndex.map {
        case (param, i) => p"case ${Lit.Int(i)} => this.$param"
      }
      Seq(
        q"""def canEqual(that: Any): Boolean =
           that.isInstanceOf[${dataInfo.dataType}]""",
        q"def productArity: Int = ${Lit.Int(dataInfo.classParams.length)}",
        q"""def productElement(n: Int): Any =
         n match {
            ..case $casesForElements
            case _ => throw new IndexOutOfBoundsException(n.toString())
         }
       """,
        q"""override def productPrefix: String =
           ${Lit.String(dataInfo.name.value)}""",
        q"""override def productIterator: Iterator[Any] =
           scala.runtime.ScalaRunTime.typedProductIterator[Any](this)"""
      )
    }
  }

  object DataHashCodeStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val hashCodeExpr = {
        def paramToHashCode(name: Term.Name) = q"$name.hashCode"

        dataInfo.classParamNames.reverse match {
          case Seq(param1) =>
            paramToHashCode(param1)
          case Seq(param1, rest @ _*) =>
            rest.foldLeft[Term](paramToHashCode(param1)) {
              case (expr, param) => q"$param.hashCode + 13 * ($expr)"
            }
        }
      }

      Seq(
        if (dataInfo.getMod("memoiseHashCode")) {
          q"override val hashCode: Int = $hashCodeExpr"
        } else {
          q"override def hashCode: Int = $hashCodeExpr"
        }
      )
    }
  }

  object DataToStringStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val paramsToString = {
        def paramToString(name: Term.Name) = q"$name.toString"

        dataInfo.classParamNames match {
          case Seq(param1) =>
            paramToString(param1)
          case Seq(param1, rest @ _*) =>
            rest.foldLeft[Term](paramToString(param1)) {
              case (expr, param) =>
                q"$expr + ${Lit.String(",")} + $param.toString"
            }
        }
      }
      val toStringBody =
        q"""${Lit.String(dataInfo.name.value + "(")} +
           $paramsToString + ${Lit.String(")")}"""

      Seq(
        if (dataInfo.getMod("memoiseToString")) {
          q"override val toString: String = $toStringBody"
        } else {
          q"override def toString: String = $toStringBody"
        }
      )
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq(
      q"override def toString: String = ${Lit.String(dataInfo.name.value)}"
    )
  }

  object DataCopyStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val newTypes =
        dataInfo.typeParams.map(tparam => Type.Name("N$" + tparam.name.value))
      val oldToNewTypes =
        dataInfo.typeParamsNames.map(_.value).zip(newTypes).toMap
      val newTypeParams: Seq[Type.Param] = newTypes.map(
        tpe =>
          Type.Param(Seq(), tpe, Seq(), Type.Bounds(None, None), Seq(), Seq())
      )
      // I couldn't find way to do it as quasiquote

      val newDataType = if (newTypeParams.nonEmpty) {
        t"${dataInfo.name}[..$newTypes]"
      } else {
        t"${dataInfo.name}"
      }

      val copyParams = dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          param"""$param: ${DataInfo.replaceType(tpe, oldToNewTypes)} =
                 this.$param"""
      }

      Seq(
        q"""def copy[..$newTypeParams](..$copyParams): $newDataType =
          ${dataInfo.dataCreating}"""
      )
    }
  }
}
