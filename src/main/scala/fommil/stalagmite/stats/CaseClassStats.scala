// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package fommil.stalagmite.stats

import fommil.stalagmite.{ DataInfo, DataStats, MetaUtils }

import scala.collection.immutable.Seq
import scala.meta._

object CaseClassStats {

  object DataApplyStats extends DataStats {

    def applyParams(dataInfo: DataInfo): Seq[Term.Param] =
      dataInfo.classParams.zip(dataInfo.classParamsTypes).map {
        case (param, tpe) =>
          param.default match {
            case Some(defaultVal) =>
              param"""${Term.Name(param.name.value)}: $tpe = $defaultVal"""
            case None =>
              param"${Term.Name(param.name.value)}: $tpe"
          }
      }

    override val statsTag: DataStats.StatsTag = DataStats.ApplyStats

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val publishing = if (dataInfo.requiresToHaveVars) {
        q"created.synchronized(created)"
      } else {
        q"created"
      }
      Seq(
        q"""def apply[..${dataInfo.simpleTypeParams}](
            ..${applyParams(dataInfo)}
          ): ${dataInfo.dataType} = {
          val created = new ${Ctor.Ref.Name(dataInfo.name.value)}(
            ..${dataInfo.classParamNames}
            )
          $publishing
        }"""
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
            that: ${dataInfo.dataType}): _root_.scala.Option[$tupleTypes] =
          _root_.scala.Some($tupleArgs)"""
      )
    }
  }

  object DataGettersStats extends DataStats {

    override val statsTag: DataStats.StatsTag = DataStats.GettersStats

    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          q"def $param: $tpe = this.${Term.Name("_" + param.value)}"
      }
  }

  object DataEqualsStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val eqs = dataInfo.classParamNames.map(
        paramName => q"this.$paramName == that.$paramName"
      )
      val eqsWithAnds = eqs match {
        case Seq(eq1) =>
          eq1
        case Seq(eq1, rest @ _*) =>
          rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
      }
      val thatEqual = if (dataInfo.dataMods.memoiseHashCode) {
        q"(this eq that) || (this.hashCode == that.hashCode && $eqsWithAnds)"
      } else {
        q"(this eq that) || ($eqsWithAnds)"
      }

      if (dataInfo.dataMods.memoise &&
          (!dataInfo.dataMods.memoiseEqualsByValue ||
          dataInfo.dataMods.memoiseStrong)) {
        Seq()
      } else {
        Seq(
          q"""override def equals(thatAny: _root_.scala.Any): _root_.scala.Boolean =
              thatAny match {
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
        q"""def canEqual(that: _root_.scala.Any): _root_.scala.Boolean =
           that.isInstanceOf[${dataInfo.dataType}]""",
        q"def productArity: _root_.scala.Int = ${Lit.Int(dataInfo.classParams.length)}",
        q"""def productElement(n: _root_.scala.Int): _root_.scala.Any =
         n match {
            ..case $casesForElements
            case _ => throw new _root_.java.lang.IndexOutOfBoundsException(n.toString())
         }
       """,
        q"""override def productPrefix: _root_.java.lang.String =
           ${Lit.String(dataInfo.name.value)}""",
        q"""override def productIterator: _root_.scala.Iterator[_root_.scala.Any] =
           _root_.scala.runtime.ScalaRunTime.typedProductIterator[_root_.scala.Any](this)"""
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
        if (dataInfo.dataMods.memoiseHashCodeLazy) {
          q"@_root_.scala.transient override lazy val hashCode: _root_.scala.Int = $hashCodeExpr"
        } else if (dataInfo.dataMods.memoiseHashCode) {
          q"@_root_.scala.transient override val hashCode: _root_.scala.Int = $hashCodeExpr"
        } else {
          q"override def hashCode: _root_.scala.Int = $hashCodeExpr"
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
        if (dataInfo.dataMods.memoiseToStringLazy) {
          q"@_root_.scala.transient override lazy val toString: _root_.java.lang.String = $toStringBody"
        } else if (dataInfo.dataMods.memoiseToString) {
          q"@_root_.scala.transient override val toString: _root_.java.lang.String = $toStringBody"
        } else {
          q"override def toString: _root_.java.lang.String = $toStringBody"
        }
      )
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq(
      q"override def toString: _root_.java.lang.String = ${Lit.String(dataInfo.name.value)}"
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
          param"""$param: ${MetaUtils.replaceType(tpe, oldToNewTypes)} =
                 this.$param"""
      }

      Seq(
        q"""def copy[..$newTypeParams](..$copyParams): $newDataType =
          ${dataInfo.dataCreating}"""
      )
    }
  }
}
