package fommil.dataMacro

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
    lazy val typeParamsNames = simpleTypeParams.map(param => Type.Name(param.name.value))
    lazy val dataType = if (typeParams.nonEmpty) t"$name[..$typeParamsNames]" else t"$name"
    lazy val dataPatType = if (typeParams.nonEmpty) pt"$name[..$typeParamsNames]" else pt"$name"

    lazy val classTypes = classParams.map(param => param.decltpe match {
      case Some(tpe: Type) => tpe
      case _               => abort("Currently complicated Type.Args aren't supported")
    })
  }
}
