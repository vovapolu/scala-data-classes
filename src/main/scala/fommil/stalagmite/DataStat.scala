package fommil.stalagmite

import DataInfo._

import scala.collection.immutable.Seq
import scala.meta._

object DataStat {

  trait DataStatBuilder {
    def classStats(dataInfo: DataInfo): Seq[Stat]  = Seq()
    def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq()
  }

  object DataApplyBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val params = dataInfo.classParams.map(
        param =>
          param.default match {
            case Some(defaultVal) =>
              param"${Term.Name(param.name.value)}: ${param.decltpe.get} = $defaultVal"
            case None =>
              param"${Term.Name(param.name.value)}: ${param.decltpe.get}"
        }
      )

      val applyBody =
        if (dataInfo.getMod("memoise")) {
          val memoisedParams = dataInfo.classParamsWithTypes.filter {
            case (param, _) =>
              dataInfo.extraParams.memoiseRefs.contains(param.value)
          }.map {
            case (param, tpe) =>
              q"""val ${Pat.Var.Term(Term.Name(param.value + "_memoised"))} =
                 memoisedRef_cache.intern($param).asInstanceOf[$tpe]"""
          }

          val argsWithMemoised = dataInfo.classParamNames.map(
            param =>
              if (dataInfo.extraParams.memoiseRefs.contains(param.value)) {
                Term.Name(param.value + "_memoised")
              } else {
                param
            }
          )

          val interning = if (dataInfo.getMod("memoiseStrong")) {
            val wrapperCreating = if (dataInfo.typeParams.nonEmpty) {
              q"new ${Ctor.Ref.Name(dataInfo.name.value + "WithValueEquality")}[..${dataInfo.typeParamsNames}](safe)"
            } else {
              q"new ${Ctor.Ref.Name(dataInfo.name.value + "WithValueEquality")}(safe)"
            }
            q"memoised_cache.intern($wrapperCreating).d"
          } else {
            q"memoised_cache.intern(safe)"
          }

          memoisedParams ++ Seq(
            q"val created = new ${Ctor.Ref.Name(dataInfo.name.value)}(..$argsWithMemoised)",
            q"val safe = created.synchronized(created)",
            interning
          )
        } else {
          Seq(
            q"val created = new ${Ctor.Ref.Name(dataInfo.name.value)}(..${dataInfo.classParamNames})",
            q"created.synchronized(created)"
          )
        }

      Seq(
        q"""def apply[..${dataInfo.simpleTypeParams}](..$params): ${dataInfo.dataType} = {
          ..$applyBody
        }"""
      )
    }
  }

  object DataUnapplyBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val clFields = dataInfo.classParams.map(
        param => q"that.${Term.Name(param.name.value)}"
      )

      val tupleTypes = if (dataInfo.classParams.length > 1) {
        t"(..${dataInfo.classParamsTypes})"
      } else {
        t"${dataInfo.classParamsTypes.head}"
      }
      val tupleArgs = if (clFields.length > 1) {
        q"(..$clFields)"
      } else {
        clFields.head
      }
      Seq(
        q"""def unapply[..${dataInfo.simpleTypeParams}](that: ${dataInfo.dataType}): Option[$tupleTypes] =
         Some($tupleArgs)"""
      )
    }
  }

  object DataGettersBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          q"def $param: $tpe = this.${Term.Name("_" + param.value)}"
      }
  }

  object DataEqualsBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val eqs = dataInfo.classParamNames.map(
        paramName => q"that.$paramName == this.$paramName"
      )
      val eqsWithAnds = eqs match {
        case Seq(eq1) => eq1
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

  object DataProductMethodsBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val casesForElements = dataInfo.classParamNames.zipWithIndex.map {
        case (param, i) => p"case ${Lit.Int(i)} => this.$param"
      }
      Seq(
        q"def canEqual(that: Any): Boolean = that.isInstanceOf[${dataInfo.dataType}]",
        q"def productArity: Int = ${Lit.Int(dataInfo.classParams.length)}",
        q"""def productElement(n: Int): Any =
         n match {
            ..case $casesForElements
            case _ => throw new IndexOutOfBoundsException(n.toString())
         }
       """,
        q"override def productPrefix: String = ${Lit.String(dataInfo.name.value)}",
        q"override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)"
      )
    }
  }

  object DataHashCodeBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val hashCodeExpr = {
        def paramToHashCode(name: Term.Name) = q"$name.hashCode"

        dataInfo.classParamNames.reverse match {
          case Seq(param1) => paramToHashCode(param1)
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

  object DataToStringBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val paramsToString = {
        def paramToString(name: Term.Name) = q"$name.toString"

        dataInfo.classParamNames match {
          case Seq(param1) => paramToString(param1)
          case Seq(param1, rest @ _*) =>
            rest.foldLeft[Term](paramToString(param1)) {
              case (expr, param) =>
                q"$expr + ${Lit.String(",")} + $param.toString"
            }
        }
      }
      val toStringBody =
        q"${Lit.String(dataInfo.name.value + "(")} + $paramsToString + ${Lit.String(")")}"

      Seq(
        if (dataInfo.getMod("memoiseToString")) {
          q"override val toString: String = $toStringBody"
        } else {
          q"override def toString: String = $toStringBody"
        }
      )
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      Seq(
        q"override def toString: String = ${Lit.String(dataInfo.name.value)}"
      )
  }

  object DataCopyBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val copyParams = dataInfo.classParamsWithTypes.map {
        case (param, tpe) => param"$param: $tpe = this.$param"
      }

      Seq(
        q"""def copy[..${dataInfo.simpleTypeParams}](..$copyParams): ${dataInfo.dataType} =
            ${dataInfo.termName}(..${dataInfo.classParamNames})"""
      )
    }
  }

  object DataWriteObjectBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val writes = dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          def writePrimitive(t: Type.Name) =
            q"out.${Term.Name("write" + t.value)}($param)"
          tpe match {
            case t @ (t"Boolean" | t"Byte" | t"Short" | t"Char" | t"Int" |
                t"Long" | t"Float" | t"Double") =>
              t match {
                case t: Type.Name => writePrimitive(t)
              }
            case t"String" => q"out.writeUTF($param)"
            case _ =>
              if (dataInfo.getMod("checkSerializable")) {
                q"out.writeObject($param: Serializable)"
              } else {
                q"out.writeObject($param)"
              }
          }
      }
      Seq(q"""
          @throws[_root_.java.io.IOException]
          private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
            ..$writes
          }
        """)
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      Seq(q"""
          @throws[_root_.java.io.IOException]
          private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()
        """)
  }

  object DataReadObjectBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val reads = dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          q"${Term.Name { "_" + param.value }} = ${tpe match {
            case t"Boolean" => q"in.readBoolean()"
            case t"Byte"    => q"in.readByte()"
            case t"Short"   => q"in.readShort()"
            case t"Char"    => q"in.readChar()"
            case t"Int"     => q"in.readInt()"
            case t"Long"    => q"in.readLong()"
            case t"Float"   => q"in.readFloat()"
            case t"Double"  => q"in.readDouble()"
            case t"String"  => q"in.readUTF()"
            case _          => q"in.readObject().asInstanceOf[$tpe]"
          }}"
      }
      Seq(q"""
        @throws[_root_.java.io.IOException]
        @throws[_root_.java.lang.ClassNotFoundException]
        private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
          ..$reads
        }
        """)
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      Seq(q"""
        @throws[_root_.java.io.IOException]
        @throws[_root_.java.lang.ClassNotFoundException]
        private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()
        """)
  }

  object DataReadResolveBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      Seq(q"""
            @throws[_root_.java.io.ObjectStreamException]
            private[this] def readResolve(): Any = ${dataInfo.dataCreating}
          """)

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      Seq(q"""
            @throws[_root_.java.io.ObjectStreamException]
            private[this] def readResolve(): Any = ${dataInfo.termName}
          """)
  }

  object DataShapelessBaseBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {

      val typeSymbols = dataInfo.classParamNames.map(
        name => q"""val ${Pat.Var.Term(Term.Name(name.value + "_tpe"))} =
            Symbol(${Lit.String(name.value)}).narrow"""
      )

      Seq(
        q"import _root_.shapeless.{::, HNil, Generic, LabelledGeneric, Typeable, TypeCase}",
        q"import _root_.shapeless.labelled.{FieldType, field}",
        q"import _root_.shapeless.syntax.singleton._"
      ) ++ typeSymbols
    }
  }

  object DataShapelessTypeableBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val typeableName = Term.Name("Typeable" + dataInfo.name.value)

      def isTypeParamOrHasTypeParams(tpe: Type) =
        tpe match {
          case tname: Type.Name =>
            dataInfo.typeParams.map(_.name.value).contains(tname.value)
          case _ =>
            true // FIXME there are more cases, but it's working with basic types at least
        }

      val distinctClassParamsTypes = Seq(
        dataInfo.classParamsTypes
          .groupBy(_.toString())
          .values
          .map(_.head)
          .toList: _*
      )

      val implicitTypeables = distinctClassParamsTypes
        .filter(isTypeParamOrHasTypeParams)
        .map(tpe => param"implicit ${Term.Name(s"T$tpe")}: Typeable[$tpe]")
      val typeCases = distinctClassParamsTypes
        .filter(isTypeParamOrHasTypeParams)
        .map(
          tpe => q"val ${Pat.Var.Term(Term.Name(s"TC_$tpe"))} = TypeCase[$tpe]"
        )
      val dataWithTypeCases = {
        val args = dataInfo.classParamsWithTypes.map {
          case (param, tpe) =>
            if (isTypeParamOrHasTypeParams(tpe)) {
              p"${Term.Name(s"TC_$tpe")}(${Pat.Var.Term(param)})"
            } else {
              p"${Pat.Var.Term(param)}"
            }
        }
        p"${dataInfo.termName}(..$args)"
      }

      val describe = {
        def getTypeDescribe(tpe: Type): Term =
          if (isTypeParamOrHasTypeParams(tpe)) {
            q"${Term.Name(s"T$tpe")}.describe"
          } else {
            Lit.String(tpe.toString())
          }

        val targs = {
          dataInfo.classParamsTypes match {
            case Seq(t1) => getTypeDescribe(t1)
            case Seq(t1, rest @ _*) =>
              rest.foldLeft[Term](getTypeDescribe(t1)) {
                case (expr, tname) =>
                  q"$expr + ${Lit.String(",")} + ${getTypeDescribe(tname)}"
              }
          }
        }
        q"${Lit.String(dataInfo.name.value + "[")} + $targs + ${Lit.String("]")}"
      }

      val typeableBody =
        q"""
           new Typeable[${dataInfo.dataType}] {
             override def cast(t: Any): Option[${dataInfo.dataType}] = {
               ..$typeCases
               t match {
                 case f @ $dataWithTypeCases => Some(${dataInfo.dataCreating})
                 case _                      => None
               }
             }
             override def describe: String = $describe
           }
          """

      if (implicitTypeables.nonEmpty) {
        Seq(q"""
          implicit def $typeableName[..${dataInfo.simpleTypeParams}]
            (..$implicitTypeables): Typeable[${dataInfo.dataType}] = $typeableBody
          """)
      } else {
        Seq(
          q"""
          implicit def $typeableName[..${dataInfo.simpleTypeParams}]: Typeable[${dataInfo.dataType}] = $typeableBody"""
        )
      }
    }
  }

  object DataShapelessGenericsBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val genericName = Term.Name("Generic" + dataInfo.name.value)
      val genericWithTypes = if (dataInfo.simpleTypeParams.nonEmpty) {
        q"$genericName[..${dataInfo.typeParamsNames}]"
      } else {
        genericName
      }
      val labelledGenericName =
        Term.Name("LabelledGeneric" + dataInfo.name.value)
      val labelledGenericWithTypes = if (dataInfo.typeParamsNames.nonEmpty) {
        q"$labelledGenericName[..${dataInfo.typeParamsNames}]"
      } else {
        labelledGenericName
      }

      val reprType =
        dataInfo.classParamsTypes.foldRight[Type](Type.Name("HNil")) {
          case (tpe, expr) => t"$tpe :: $expr"
        }
      val labelledReprType =
        dataInfo.classParamsWithTypes.foldRight[Type](Type.Name("HNil")) {
          case ((param, tpe), expr) =>
            t"FieldType[${Term.Name(param.value + "_tpe")}.type, $tpe] :: $expr"
        }

      val labelledFields =
        dataInfo.classParamNames.foldRight[Term](Term.Name("HNil")) {
          case (param, expr) =>
            q"field[${Term.Name(param.value + "_tpe")}.type](f.$param) :: $expr"
        }
      val fields = dataInfo.classParamNames.foldRight[Pat](Term.Name("HNil")) {
        case (param, expr) => p"${Pat.Var.Term(param)} :: $expr"
      }

      val generic =
        q"""
        implicit def $genericName[..${dataInfo.simpleTypeParams}]:
          Generic.Aux[${dataInfo.dataType}, $reprType] =
          new Generic[${dataInfo.dataType}] {
            override type Repr = $reprType
            override def to(f: ${dataInfo.dataType}): Repr = $labelledGenericWithTypes.to(f)
            override def from(r: Repr): ${dataInfo.dataType} = r match {
              case ($fields) => ${dataInfo.dataCreating}
            }
        }"""

      val labelledGeneric =
        q"""
        implicit def $labelledGenericName[..${dataInfo.simpleTypeParams}]:
          LabelledGeneric.Aux[${dataInfo.dataType}, $labelledReprType] =
          new LabelledGeneric[${dataInfo.dataType}] {
            override type Repr = $labelledReprType
            override def to(f: ${dataInfo.dataType}): Repr = $labelledFields
            override def from(r: Repr): ${dataInfo.dataType} = $genericWithTypes.from(r)
          }
          """

      Seq(generic, labelledGeneric)
    }
  }

  object DataMemoiseBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      if (dataInfo.getMod("memoiseStrong")) {
        Seq()
      } else {
        Seq(
          q"def intern: ${dataInfo.dataType} = ${dataInfo.dataCreating}"
        )
      }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      if (dataInfo.getMod("memoiseStrong")) {
        val wrapperName = Type.Name(dataInfo.name.value + "WithValueEquality")
        val wrapperWildcardType = if (dataInfo.typeParams.nonEmpty) {
          t"$wrapperName[..${Seq.fill(dataInfo.typeParams.length)(t"_")}]"
        } else {
          wrapperName
        }

        val dataWrapper = {
          val wrapperPatType = if (dataInfo.typeParams.nonEmpty) {
            pt"$wrapperName[..${Seq.fill(dataInfo.typeParams.length)(pt"_")}]"
          } else {
            pt"$wrapperName"
          }
          val wrapperCaseBody = {
            val eqs = dataInfo.classParamNames.map(
              paramName => q"that.d.$paramName == this.d.$paramName"
            )
            eqs match {
              case Seq(eq1) => eq1
              case Seq(eq1, rest @ _*) =>
                rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
            }
          }
          val wrapperCase = if (dataInfo.getMod("memoiseHashCode")) {
            p"case that: $wrapperPatType if this.hashCode == that.hashCode => $wrapperCaseBody"
          } else {
            p"case that: $wrapperPatType => $wrapperCaseBody"
          }
          q"""private class $wrapperName[..${dataInfo.simpleTypeParams}](val d: ${dataInfo.dataType}) {
               override def toString: String = d.toString
               override def hashCode: Int = d.hashCode
               override def equals(o: Any): Boolean = o match {
                 case $wrapperCase
                 case _ => false
               }
             }"""
        }

        Seq(
          dataWrapper,
          q"""private[this] val memoised_cache =
              _root_.com.google.common.collect.Interners.newStrongInterner[$wrapperWildcardType]()"""
        ) ++ (if (dataInfo.extraParams.memoiseRefs.nonEmpty) {
                Seq(
                  q"""private[this] val memoisedRef_cache =
             _root_.com.google.common.collect.Interners.newStrongInterner[AnyRef]()"""
                )
              } else {
                Seq()
              })
      } else {
        val dataWildCardType = if (dataInfo.typeParams.nonEmpty) {
          t"${Type.Name(dataInfo.name.value)}[..${Seq.fill(dataInfo.typeParams.length)(t"_")}]"
        } else {
          Type.Name(dataInfo.name.value)
        }

        Seq(
          q"""private[this] val memoised_cache =
              _root_.com.google.common.collect.Interners.newWeakInterner[$dataWildCardType]()"""
        ) ++ (if (dataInfo.extraParams.memoiseRefs.nonEmpty) {
                Seq(
                  q"""private[this] val memoisedRef_cache =
             _root_.com.google.common.collect.Interners.newWeakInterner[AnyRef]()"""
                )
              } else {
                Seq()
              })
      }
  }
}
