package fommil.stalagmite.stats

import fommil.stalagmite.DataInfo
import fommil.stalagmite.DataStats

import scala.collection.immutable.Seq
import scala.meta._

object SerializableStats {

  object DataWriteObjectStats extends DataStats {
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
            case t"String" =>
              q"out.writeUTF($param)"
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
          private[this] def writeObject(
            out: java.io.ObjectOutputStream): Unit = {
            ..$writes
          }
        """)
    }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      Seq(q"""
          @throws[_root_.java.io.IOException]
          private[this] def writeObject(
            out: java.io.ObjectOutputStream): Unit = ()
        """)
  }

  object DataReadObjectStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = {
      def constructReadTarget(param: Term.Name, read: Term) =
        if (dataInfo.requiredToPack) {
          q"val ${Pat.Var.Term(param)} = $read"
        } else {
          q"${Term.Name("_" + param.value)} = $read"
        }

      val reads = dataInfo.classParamsWithTypes.map {
        case (param, tpe) =>
          constructReadTarget(
            param,
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
          )
      }

      val optionalPack = if (dataInfo.requiredToPack) {
        val pack =
          q"""
              val packed = ${dataInfo.termName}
                .pack(..${dataInfo.classParamNames})
          """
        val fieldsAssignments = dataInfo.optimizedParams.unzip._1 match {
          case Seq(p1) => Seq(q"${Term.Name("_" + p1.value)} = packed")
          case ps =>
            ps.zipWithIndex.map {
              case (param, ind) =>
                q"""${Term.Name("_" + param.value)} =
                   packed.${Term.Name("_" + ind)}"""
            }
        }
        pack +: fieldsAssignments
      } else {
        Seq()
      }

      Seq(q"""
        @throws[_root_.java.io.IOException]
        @throws[_root_.java.lang.ClassNotFoundException]
        private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
          ..$reads
          ..$optionalPack
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

  object DataReadResolveStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] = Seq(
      q"""
          @throws[_root_.java.io.ObjectStreamException]
          private[this] def readResolve(): Any = ${dataInfo.dataCreating}
      """
    )

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq(
      q"""
          @throws[_root_.java.io.ObjectStreamException]
          private[this] def readResolve(): Any = ${dataInfo.termName}
        """
    )
  }
}
