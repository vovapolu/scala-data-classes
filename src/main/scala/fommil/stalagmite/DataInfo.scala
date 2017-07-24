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
          replaceType(tApply2, transform)
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

  def isPrimitiveType(tpe: Type): Boolean =
    tpe match {
      case t"Boolean" | t"Byte" | t"Short" | t"Char" | t"Int" | t"Long" |
          t"Float" | t"Double" =>
        true
      case _ => false
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

  // heap optimization methods

  case class BitPosition(optionBit: Option[Int], booleanBit: Option[Int])

  lazy val bitPositions: List[BitPosition] = {
    val tempSizes = classParamsTypes.toList.map(tpe => {
      val (optionBit, typeWithoutOption) = tpe match {
        case t"Option[$t]"
            if DataInfo.isPrimitiveType(t) &&
              getMod("optimiseHeapOptions") =>
          (1, t)
        case t =>
          (0, t)
      }

      val booleanBit = typeWithoutOption match {
        case t"Boolean" if getMod("optimiseHeapBooleans") => 1
        case _                                            => 0
      }

      (optionBit, booleanBit)
    })

    def generateBitPosition(curSizes: List[(Int, Int)],
                            curReservedBit: Int): List[BitPosition] =
      curSizes match {
        case head :: tail =>
          head match {
            case (optionBit, booleanBit) =>
              BitPosition(
                if (optionBit > 0) Some(curReservedBit) else None,
                if (booleanBit > 0) Some(curReservedBit + optionBit)
                else None,
              ) :: generateBitPosition(
                tail,
                curReservedBit + optionBit + booleanBit,
              )
          }
        case _ => List.empty
      }

    generateBitPosition(tempSizes, 0)
  }

  lazy val hasBitmask: Boolean = classParamsTypes.exists {
    case t"Option[$t]" if getMod("optimiseHeapOptions") => true
    case t"Boolean" if getMod("optimiseHeapBooleans")   => true
    case _                                              => false
  }

  lazy val requiredToPack: Boolean = classParamsTypes.exists {
    case t"Option[$t]" if getMod("optimiseHeapOptions") => true
    case t"Boolean" if getMod("optimiseHeapBooleans")   => true
    case t"String" if getMod("optimiseHeapStrings")     => true
    case _                                              => false
  }

  lazy val optimizedParams: Seq[(Term.Name, Type)] = {
    val transformedParams = classParamsWithTypes.flatMap {
      case (param, tpe) =>
        val typeWithoutOption = tpe match {
          case t"Option[$t]" if getMod("optimiseHeapOptions") => t
          case t                                              => t
        }

        val transformedType = typeWithoutOption match {
          case t"Boolean" if getMod("optimiseHeapBooleans") => None
          case t"String" if getMod("optimiseHeapStrings") =>
            Some(t"Array[Byte]")
          case t => Some(t)
        }

        transformedType.map((param, _))
    }

    transformedParams ++ (
      if (hasBitmask) {
        Seq((Term.Name("bitmask"), t"Long"))
      } else {
        Seq()
      }
    )
  }

  def getMod(mod: String): Boolean = dataMods.getOrElse(mod, false)
}
