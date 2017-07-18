// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package fommil.stalagmite

import scala.collection.immutable.Seq
import scala.meta._

object DataInfo {
  def replaceTypeName(tpe: Type.Name,
                      transform: Map[String, Type.Name]): Type.Name =
    transform.getOrElse(tpe.value, tpe)

  def replaceType(tpe: Type, transform: Map[String, Type.Name]): Type =
    tpe match {
      case t: Type.Name => replaceTypeName(t, transform)
      case Type.Apply(tApply, tsApplied) =>
        Type.Apply(
          replaceType(tApply, transform),
          tsApplied.map(replaceType(_, transform))
        )
      case Type.ApplyInfix(tApply1, tInfix, tApply2) =>
        Type.ApplyInfix(
          replaceType(tApply1, transform),
          replaceTypeName(tInfix, transform),
          replaceType(tApply2, transform),
        )
      case Type.With(tWith1, tWith2) =>
        Type.With(
          replaceType(tWith1, transform),
          replaceType(tWith2, transform)
        )
      case Type.Function(tArgs, tOut) =>
        Type.Function(
          tArgs.map(replaceTypeArg(_, transform)),
          replaceType(tOut, transform)
        )
      case Type.Tuple(ts) =>
        Type.Tuple(
          ts.map(replaceType(_, transform))
        )
      case _ => tpe // only basic cases for now
    }

  def replaceTypeArg(typeArg: Type.Arg,
                     transform: Map[String, Type.Name]): Type.Arg =
    typeArg match {
      case Type.Arg.ByName(tpe) =>
        Type.Arg.ByName(replaceType(tpe, transform))
      case Type.Arg.Repeated(tpe) =>
        Type.Arg.Repeated(replaceType(tpe, transform))
      case t: Type =>
        replaceType(t, transform)
      case _ => typeArg
    }
}

final case class ExtraParams(memoiseRefs: Seq[String] = Seq())

final case class DataInfo(name: Type.Name,
                          classParams: Seq[Term.Param],
                          typeParams: Seq[Type.Param],
                          dataMods: Map[String, Boolean],
                          extraParams: ExtraParams = ExtraParams()) {
  lazy val simpleTypeParams: Seq[Type.Param] = typeParams.map(
    tparam => tparam.copy(mods = Seq(), tbounds = Type.Bounds(None, None))
  )
  lazy val typeParamsNames: Seq[Type.Name] =
    typeParams.map(param => Type.Name(param.name.value))
  lazy val dataType: Type = if (typeParams.nonEmpty) {
    t"$name[..$typeParamsNames]"
  } else {
    t"$name"
  }
  lazy val dataPatType: Pat.Type = if (typeParams.nonEmpty) {
    pt"$name[..${Seq.fill(typeParams.length)(pt"_")}]"
  } else {
    pt"$name"
  }

  lazy val classParamsTypes: Seq[Type] = classParams.map(_.decltpe match {
    case Some(tpe: Type) => tpe
    case _               => abort("Currently complicated Type.Args aren't supported")
  })

  lazy val classParamNames: Seq[Term.Name] =
    classParams.map(param => Term.Name(param.name.value))
  lazy val classParamsWithTypes: Seq[(Term.Name, Type)] =
    classParamNames.zip(classParamsTypes)

  lazy val termName     = Term.Name(name.value)
  lazy val dataCreating = q"$termName(..$classParamNames)"

  case class BitPosition(optionBit: Option[Int],
                         booleanBit: Option[Int],
                         fieldByte: Option[Int],
                         fieldSize: Option[Int])

  lazy val bitPositions: List[BitPosition] = {
    val tempSizes = classParamsTypes.toList.map(tpe => {
      val (optionBit, typeWithoutOption) = tpe match {
        case t"Option[$t]" if getMod("optimiseHeapOptions") => (1, t)
        case t                                              => (0, t)
      }

      val booleanBit = typeWithoutOption match {
        case t"Boolean" if getMod("optimiseHeapBooleans") => 1
        case _                                            => 0
      }

      val fieldByteSize = if (getMod("optimiseHeapPrimitives")) {
        typeWithoutOption match {
          case t"Byte"   => 1
          case t"Char"   => 1
          case t"Short"  => 2
          case t"Int"    => 4
          case t"Float"  => 4
          case t"Long"   => 8
          case t"Double" => 8
          case _         => 0
        }
      } else {
        0
      }

      (optionBit, booleanBit, fieldByteSize)
    })

    val booleansAndOptionsBits = tempSizes.map(s => s._1 + s._2).sum
    val reservedBytes = booleansAndOptionsBits / 8 +
      (if (booleansAndOptionsBits % 8 != 0) 1 else 0)

    def generateBitPosition(curSizes: List[(Int, Int, Int)],
                            curReservedBit: Int,
                            curByte: Int): List[BitPosition] =
      curSizes match {
        case head :: tail =>
          head match {
            case (optionBit, booleanBit, fieldByteSize) =>
              BitPosition(
                if (optionBit > 0) Some(curReservedBit) else None,
                if (booleanBit > 0) Some(curReservedBit + optionBit)
                else None,
                if (fieldByteSize > 0) Some(curByte) else None,
                if (fieldByteSize > 0) Some(fieldByteSize) else None
              ) :: generateBitPosition(
                tail,
                curReservedBit + optionBit + booleanBit,
                curByte + fieldByteSize
              )
          }
        case _ => List.empty
      }

    generateBitPosition(tempSizes, 0, reservedBytes)
  }

  def getMod(mod: String): Boolean = dataMods.getOrElse(mod, false)
}
