package fommil.data.impl

import fommil.data.impl.DataInfo.DataInfo

import scala.collection.immutable.Seq
import scala.meta._

object DataStat {

  trait DataStatBuilder {
    def classStats(dataInfo: DataInfo): Seq[Stat] = Seq()
    def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq()
  }

  object DataApplyBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val args = dataInfo.classParams.map(param => Term.Name(param.name.value))
      val params = dataInfo.classParams.map(param =>
        param.default match {
          case Some(defaultVal) => param"${Term.Name(param.name.value)}: ${param.decltpe.get} = $defaultVal"
          case None             => param"${Term.Name(param.name.value)}: ${param.decltpe.get}"
        })

      Seq(
        if (dataInfo.getMod("intern")) {
          q"""def apply[..${dataInfo.simpleTypeParams}](..$params): ${dataInfo.dataType} = {
            val newVal = new ${Ctor.Ref.Name(dataInfo.name.value)}(..$args)
            ${Term.Name(dataInfo.name.value)}.intern(newVal)
          }"""
        } else {
          q"""def apply[..${dataInfo.simpleTypeParams}](..$params): ${dataInfo.dataType} = {
              val newVal = new ${Ctor.Ref.Name(dataInfo.name.value)}(..$args)
              newVal.synchronized(newVal)
            }"""
        }
      )
    }
  }

  object DataUnapplyBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val clFields = dataInfo.classParams.map(param => q"that.${Term.Name(param.name.value)}")

      val tupleTypes = if (dataInfo.classParams.length > 1) {
        t"(..${dataInfo.classTypes})"
      } else {
        t"${dataInfo.classTypes.head}"
      }
      val tupleArgs = if (clFields.length > 1) {
        q"(..$clFields)"
      } else {
        clFields.head
      }
      Seq(q"""def unapply[..${dataInfo.simpleTypeParams}](that: ${dataInfo.dataType}): Option[$tupleTypes] =
         Some($tupleArgs)""")
    }
  }

  object DataGettersBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      dataInfo.classParams.zip(dataInfo.classTypes).map {
        case (param, tpe) =>
          q"def ${Term.Name(param.name.value)}: $tpe = this.${Term.Name("_" + param.name.value)}"
      }
    }
  }

  object DataEqualsBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val eqs = dataInfo.classParams.map(param =>
        q"that.${Term.Name(param.name.value)} == this.${Term.Name(param.name.value)}")
      val eqsWithAnds = eqs match {
        case Seq(eq1)            => eq1
        case Seq(eq1, rest @ _*) => rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
      }

      Seq(
        if (dataInfo.getMod("intern")) {
          q"override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object])"
        } else if (dataInfo.getMod("idEquals")) {
          q"""override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
            (thatAny match {
              case that: ${dataInfo.dataPatType} =>
                val idTuple = if (this.id > that.id) (that.id, this.id) else (this.id, that.id)
                val res = ${dataInfo.termName}.idEquals.get(idTuple)
                if (res == null) {
                  val realEquals = $eqsWithAnds
                  ${dataInfo.termName}.idEquals.put(idTuple, Boolean.box(realEquals))
                  realEquals
                } else {
                  Boolean.unbox(res)
                }
              case _ => false
           })"""
        } else {
          q"""override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
            (thatAny match {
              case that: ${dataInfo.dataPatType} => $eqsWithAnds
              case _ => false
           })"""
        }
      )
    }
  }

  object DataProductMethodsBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val casesForElements = dataInfo.classParams.zipWithIndex.map {
        case (param, i) => p"case ${Lit.Int(i)} => this.${Term.Name(param.name.value)}"
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
      val hashCodeExpr = dataInfo.classParams.dropRight(1)
        .foldRight[Term](q"${Term.Name(dataInfo.classParams.last.name.value)}.hashCode") {
          case (param, expr) => q"${Term.Name(param.name.value)}.hashCode + 13 * ($expr)"
        }
      Seq(
        q"override def hashCode(): Int = $hashCodeExpr"
      )
    }
  }

  object DataToStringBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val paramsToString = dataInfo.classParams.drop(1)
        .foldLeft[Term](q"${Term.Name(dataInfo.classParams.head.name.value)}.toString") {
          case (expr, param) => q"$expr + ${Lit.String(",")} + ${Term.Name(param.name.value)}.toString"
        }
      Seq(
        q"""
            override def toString: String =
              ${Lit.String(dataInfo.name.value + "(")} + $paramsToString + ${Lit.String(")")}
          """
      )
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      Seq(
        q"override def toString: String = ${Lit.String(dataInfo.name.value)}"
      )
    }
  }

  object DataCopyBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val newTypes = dataInfo.typeParams.map(tparam => Type.Name("N$" + tparam.name.value))
      val oldToNewTypes = dataInfo.typeParamsNames.map(_.value).zip(newTypes).toMap
      val newTypeParams: Seq[Type.Param] = dataInfo.typeParamsNames.zip(newTypes).map {
        case (oldTpe, newTpe) =>
          Type.Param(Seq(), newTpe, Seq(), Type.Bounds(Some(oldTpe), None), Seq(), Seq())
      }
      val newDataType = if (newTypeParams.nonEmpty) {
        t"${dataInfo.name}[..$newTypes]"
      } else {
        t"${dataInfo.name}"
      }

      val copyParams = dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          param"$param: ${oldToNewTypes.getOrElse(tpe.value, tpe)} = this.$param"
      }

      Seq(
        q"""def copy[..$newTypeParams](..$copyParams): $newDataType =
          ${Term.Name(dataInfo.name.value)}(..${dataInfo.classParamNames})"""
      )
    }
  }

  object DataWriteObjectBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val writes = dataInfo.classParams.zip(dataInfo.classTypes).map {
        case (param, tpe) =>
          q"out.${
            tpe match {
              case t"Boolean" => q"writeBoolean"
              case t"Byte"    => q"writeByte"
              case t"Short"   => q"writeShort"
              case t"Char"    => q"writeChar"
              case t"Int"     => q"writeInt"
              case t"Long"    => q"writeLong"
              case t"Float"   => q"writeFloat"
              case t"Double"  => q"writeDouble"
              case t"String"  => q"writeUTF"
              case _          => q"writeObject"
            }
          }(${Term.Name(param.name.value)})"
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
      val reads = dataInfo.classParams.zip(dataInfo.classTypes).map {
        case (param, tpe) =>
          q"${Term.Name { "_" + param.name.value }} = ${
            tpe match {
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
            }
          }"
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
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val args = dataInfo.classParams.map(param => Term.Name(param.name.value))

      Seq(q"""
            @throws[_root_.java.io.ObjectStreamException]
            private[this] def readResolve(): Any = ${Term.Name(dataInfo.name.value)}(..$args)
          """)
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      Seq(q"""
            @throws[_root_.java.io.ObjectStreamException]
            private[this] def readResolve(): Any = ${Term.Name(dataInfo.name.value)}
          """)
  }

  object DataShapelessBaseBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {

      val typeSymbols = dataInfo.classParams.map(param =>
        q"val ${Pat.Var.Term(Term.Name(param.name.value + "_tpe"))} = Symbol(${Lit.String(param.name.value)}).narrow")

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
      val implicitTypeables = dataInfo.typeParams.map(tparam =>
        param"implicit ${Term.Name("T" + tparam.name.value)}: Typeable[${Type.Name(tparam.name.value)}]")

      val typeCases = dataInfo.typeParams.map(tparam =>
        q"val ${Pat.Var.Term(Term.Name("TC_" + tparam.name.value))} = TypeCase[${Type.Name(tparam.name.value)}]")
      val dataWithTypeCases = {
        val args = dataInfo.classParams.zip(dataInfo.classTypeNames).map {
          case (param, tpe) =>
            if (dataInfo.typeParams.map(_.name.value).contains(tpe.value)) {
              p"${Term.Name("TC_" + tpe.value)}(${Pat.Var.Term(Term.Name(param.name.value))})"
            } else {
              p"${Pat.Var.Term(Term.Name(param.name.value))}"
            }
        }
        p"${dataInfo.termName}(..$args)"
      }
      val dataCreating = q"${dataInfo.termName}(..${dataInfo.classParams.map(param => Term.Name(param.name.value))})"

      val describe = {
        def getTypeDescribe(tpe: Type.Name): Term =
          if (dataInfo.typeParams.map(_.name.value).contains(tpe.value)) {
            q"${Term.Name("T" + tpe.value)}.describe"
          } else {
            Lit.String(tpe.value)
          }

        val targs = dataInfo.classTypeNames.drop(1)
          .foldLeft[Term](getTypeDescribe(dataInfo.classTypeNames.head)) {
            case (expr, tpe) => q"$expr + ${Lit.String(",")} + ${getTypeDescribe(tpe)}"
          }
        q"${Lit.String(dataInfo.name.value + "[")} + $targs + ${Lit.String("]")}"
      }

      Seq(q"""
        implicit def $typeableName[..${dataInfo.simpleTypeParams}]
        (..$implicitTypeables): Typeable[${dataInfo.dataType}] =
          new Typeable[${dataInfo.dataType}] {
            override def cast(t: Any): Option[${dataInfo.dataType}] = {
              ..$typeCases
              t match {
                case f @ $dataWithTypeCases => Some($dataCreating)
                case _                      => None
              }
            }
            override def describe: String = $describe
          }
        """)
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
      val labelledGenericName = Term.Name("LabelledGeneric" + dataInfo.name.value)
      val labelledGenericWithTypes = if (dataInfo.typeParamsNames.nonEmpty) {
        q"$labelledGenericName[..${dataInfo.typeParamsNames}]"
      } else {
        labelledGenericName
      }

      val reprType =
        dataInfo.classTypeNames.foldRight[Type](Type.Name("HNil")) { case (tpe, expr) => t"$tpe :: $expr" }
      val labelledReprType =
        dataInfo.classParams.zip(dataInfo.classTypeNames).foldRight[Type](Type.Name("HNil")) {
          case ((param, tpe), expr) => t"FieldType[${Term.Name(param.name.value + "_tpe")}.type, $tpe] :: $expr"
        }

      val labelledFields =
        dataInfo.classParams.foldRight[Term](Term.Name("HNil")) {
          case (param, expr) =>
            q"field[${Term.Name(param.name.value + "_tpe")}.type](f.${Term.Name(param.name.value)}) :: $expr"
        }
      val fields = dataInfo.classParams.foldRight[Pat](Term.Name("HNil")) {
        case (param, expr) => p"${Pat.Var.Term(Term.Name(param.name.value))} :: $expr"
      }
      val dataCreating = q"${dataInfo.termName}(..${dataInfo.classParams.map(param => Term.Name(param.name.value))})"

      val generic =
        q"""
        implicit def $genericName[..${dataInfo.simpleTypeParams}]:
          Generic.Aux[${dataInfo.dataType}, $reprType] =
          new Generic[${dataInfo.dataType}] {
            override type Repr = $reprType
            override def to(f: ${dataInfo.dataType}): Repr = $labelledGenericWithTypes.to(f)
            override def from(r: Repr): ${dataInfo.dataType} = r match {
              case ($fields) => $dataCreating
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

  //  private[dataMacro] def buildInternMap(name: Type.Name, dataParams: Seq[DataParam]): Seq[Stat] = {
  //    val wrapperName = Type.Name(name.value + "Wrapper")
  //    val eqs = dataParams.map(param => q"that.cl.${Term.Name(param.name)} == this.cl.${Term.Name(param.name)}")
  //    val eqsWithAnds = eqs match {
  //      case Seq(eq1) => eq1
  //      case Seq(eq1, rest @ _ *) => rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
  //    }
  //    Seq(
  //      q"""private[$name] class $wrapperName (val cl: $name) {
  //          override def hashCode(): Int = cl.hashCode
  //          override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
  //            (thatAny match {
  //              case that: $wrapperName => $eqsWithAnds
  //              case _ => false
  //            })
  //       }""",
  //      q"""private[$name] val internMap: java.util.concurrent.ConcurrentHashMap[$wrapperName, $name] =
  //         new java.util.concurrent.ConcurrentHashMap()""",
  //      q"""def intern(cl: $name): $name = {
  //          val res = internMap.putIfAbsent(new ${Ctor.Ref.Name(wrapperName.value)}(cl), cl)
  //          if (res == null) cl else res
  //      }"""
  //    )
  //  }
  //
  //  private[dataMacro] def buildIntern(name: Type.Name): Defn.Def = {
  //    q"def intern: $name = ${Term.Name(name.value)}.intern(this)"
  //  }
  //
  //  private[dataMacro] def buildIdGenerator(name: Type.Name): Seq[Defn.Val] = {
  //    Seq(
  //      q"private[$name] val idGenerator = new java.util.concurrent.atomic.AtomicInteger()",
  //      q"private[$name] val idEquals = new java.util.concurrent.ConcurrentHashMap[(Int, Int), java.lang.Boolean]()"
  //    )
  //  }
  //
  //  private[dataMacro] def buildIdVal(name: Type.Name): Defn.Val = {
  //    q"private[$name] val id: Int = ${Term.Name(name.value)}.idGenerator.incrementAndGet()"
  //  }

}
