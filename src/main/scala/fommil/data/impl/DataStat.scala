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
      val params = dataInfo.classParams.map(param => param"${Term.Name(param.name.value)}: ${param.decltpe.get}")
      Seq(
        if (dataInfo.dataMods.intern) {
          q"""def apply[..${dataInfo.simpleTypeParams}](..$params): ${dataInfo.dataType} = {
            val newVal = new ${Ctor.Ref.Name(dataInfo.name.value)}(..$args)
            ${Term.Name(dataInfo.name.value)}.intern(newVal)
          }"""
        } else {
          q"""def apply[..${dataInfo.simpleTypeParams}](..$params): ${dataInfo.dataType} =
            new ${Ctor.Ref.Name(dataInfo.name.value)}(..$args)"""
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
        if (dataInfo.dataMods.intern) {
          q"override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object])"
        } else if (dataInfo.dataMods.idEquals) {
          q"""override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
            (thatAny match {
              case that: ${dataInfo.dataPatType} =>
                val idTuple = if (this.id > that.id) (that.id, this.id) else (this.id, that.id)
                val res = ${Term.Name(dataInfo.name.value)}.idEquals.get(idTuple)
                if (res == null) {
                  val realEquals = $eqsWithAnds
                  ${Term.Name(dataInfo.name.value)}.idEquals.put(idTuple, Boolean.box(realEquals))
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
      val paramsToString = dataInfo.classParams.foldLeft[Term](
        q"${Lit.String(dataInfo.name.value)} + ${Lit.String("(")}"
      ) { case (expr, param) => q"$expr + ${Term.Name(param.name.value)}.toString" }
      Seq(
        q"override def toString(): String = $paramsToString + ${Lit.String(")")}"
      )
    }
  }

  object DataCopyBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val copyParams = dataInfo.classParams.map(
        param => Term.Param(
          Seq(), Term.Name(param.name.value), param.decltpe, Some(q"this.${Term.Name(param.name.value)}")
        )
      )
      val copyArgs = dataInfo.classParams.map(param => Term.Name(param.name.value))

      Seq(q"def copy(..$copyParams): ${dataInfo.dataType} = new ${Ctor.Ref.Name(dataInfo.name.value)}(..$copyArgs)")
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
          @throws[java.io.IOException]
          private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
            ..$writes
          }
        """)
    }
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
        @throws[java.io.IOException]
        @throws[ClassNotFoundException]
        private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
          ..$reads
        }
        """)
    }
  }

  object DataReadResolveBuilder extends DataStatBuilder {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      val args = dataInfo.classParams.map(param => Term.Name(param.name.value))

      Seq(
        q"""
            @throws[java.io.ObjectStreamException]
            private[this] def readResolve(): Any = ${Term.Name(dataInfo.name.value)}(..$args)
          """
      )
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
