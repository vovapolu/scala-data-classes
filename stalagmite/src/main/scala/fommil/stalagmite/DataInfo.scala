// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package fommil.stalagmite

import scala.collection.immutable.Seq
import scala.meta._

object DataMods {
  type |:[L, R] = Either[L, R]

  def fromPairs(pairs: Seq[(String, Boolean |: Seq[String])],
                applyDefaults: Boolean = true): DataMods = {

    def collectMod(mod: String, defaultParam: Boolean = false): Boolean =
      pairs.collect {
        case (`mod`, Left(b)) => b
      }.headOption.getOrElse(defaultParam)

    DataMods(
      collectMod("product"),
      collectMod("checkSerializable", applyDefaults),
      collectMod("companionExtends"),
      collectMod("serializable", applyDefaults),
      collectMod("shapeless", applyDefaults),
      collectMod("memoise"),
      collectMod("memoiseStrong"),
      collectMod("memoiseEqualsByValue"),
      collectMod("memoiseHashCode"),
      collectMod("memoiseHashCodeLazy"),
      collectMod("memoiseToString"),
      collectMod("memoiseToStringLazy"),
      collectMod("optimiseHeapOptions"),
      collectMod("optimiseHeapBooleans"),
      collectMod("optimiseHeapStrings"),
      pairs.collect {
        case ("memoiseRefs", Right(refs)) => refs
      }.headOption.getOrElse(Seq())
    )
  }
}

final case class DataMods(product: Boolean = false,
                          checkSerializable: Boolean = true,
                          companionExtends: Boolean = false,
                          serializable: Boolean = true,
                          shapeless: Boolean = true,
                          memoise: Boolean = false,
                          memoiseStrong: Boolean = false,
                          memoiseEqualsByValue: Boolean = false,
                          memoiseHashCode: Boolean = false,
                          memoiseHashCodeLazy: Boolean = false,
                          memoiseToString: Boolean = false,
                          memoiseToStringLazy: Boolean = false,
                          optimiseHeapOptions: Boolean = false,
                          optimiseHeapBooleans: Boolean = false,
                          optimiseHeapStrings: Boolean = false,
                          memoiseRefs: Seq[String] = Seq()) {

  val strongMemoisation: Boolean = memoise && memoiseStrong
  val weakMemoisation: Boolean   = memoise && !memoiseStrong
}

final case class DataInfo(name: Type.Name,
                          classParams: Seq[Term.Param],
                          typeParams: Seq[Type.Param],
                          dataMods: DataMods) {
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

  lazy val requiresToHaveVars = requiredToPack && dataMods.serializable

  // heap optimization methods

  case class BitPosition(optionBit: Option[Int], booleanBit: Option[Int])

  lazy val bitPositions: List[BitPosition] = {
    val tempSizes = classParamsTypes.toList.map(tpe => {
      val (optionBit, typeWithoutOption) = tpe match {
        case t"Option[$t]"
            if MetaUtils.isPrimitiveType(t) &&
              dataMods.optimiseHeapOptions =>
          (1, t)
        case t =>
          (0, t)
      }

      val booleanBit = typeWithoutOption match {
        case t"Boolean" if dataMods.optimiseHeapBooleans => 1
        case _                                           => 0
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
                else None
              ) :: generateBitPosition(
                tail,
                curReservedBit + optionBit + booleanBit
              )
          }
        case _ => List.empty
      }

    generateBitPosition(tempSizes, 0)
  }

  lazy val hasBitmask: Boolean = classParamsTypes.exists {
    case t"Option[$t]" if dataMods.optimiseHeapOptions => true
    case t"Boolean" if dataMods.optimiseHeapBooleans   => true
    case _                                             => false
  }

  lazy val requiredToPack: Boolean = classParamsTypes.exists {
    case t"Option[$t]" if dataMods.optimiseHeapOptions => true
    case t"Boolean" if dataMods.optimiseHeapBooleans   => true
    case t"String" if dataMods.optimiseHeapStrings     => true
    case _                                             => false
  }

  lazy val optimizedParams: Seq[(Term.Name, Type)] = {
    val transformedParams = classParamsWithTypes.flatMap {
      case (param, tpe) =>
        val typeWithoutOption = tpe match {
          case t"Option[$t]" if dataMods.optimiseHeapOptions => t
          case t                                             => t
        }

        val transformedType = typeWithoutOption match {
          case t"Boolean" if dataMods.optimiseHeapBooleans => None
          case t"String" if dataMods.optimiseHeapStrings =>
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
}
