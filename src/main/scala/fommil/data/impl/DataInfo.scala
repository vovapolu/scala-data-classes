package fommil.data.impl

import scala.collection.immutable.Seq
import scala.meta._

object DataInfo {
  case class DataMods(intern: Boolean, idEquals: Boolean)

  case class DataInfo(
    name: Type.Name,
    classParams: Seq[Term.Param],
    typeParams: Seq[Type.Param],
    dataMods: DataMods
  ) {

    lazy val simpleTypeParams = typeParams.map(
      tparam => tparam.copy(mods = Seq(), tbounds = Type.Bounds(None, None))
    )
    lazy val typeParamsNames = typeParams.map(param => Type.Name(param.name.value))
    lazy val dataType = if (typeParams.nonEmpty) t"$name[..$typeParamsNames]" else t"$name"
    lazy val dataPatType = if (typeParams.nonEmpty) pt"$name[..${Seq.fill(typeParams.length)(pt"_")}]" else pt"$name"

    lazy val classTypes = classParams.map(_.decltpe match {
      case Some(tpe: Type) => tpe
      case _               => abort("Currently complicated Type.Args aren't supported")
    })

    lazy val classTypeNames = classParams.map(_.decltpe match {
      case Some(tpe: Type.Name) => tpe
      case _                    => abort("Currently only Type.Name as field type is supported")
    })

    lazy val termName = Term.Name(name.value)
  }
}
