// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package fommil.stalagmite

import scala.collection.immutable.Seq
import scala.meta._

object DataInfo {
  case class ExtraParams(memoiseRefs: Seq[String] = Seq())

  case class DataInfo(
    name: Type.Name,
    classParams: Seq[Term.Param],
    typeParams: Seq[Type.Param],
    dataMods: Map[String, Boolean],
    extraParams: ExtraParams = ExtraParams()
  ) {
    lazy val simpleTypeParams = typeParams.map(
      tparam => tparam.copy(mods = Seq(), tbounds = Type.Bounds(None, None))
    )
    lazy val typeParamsNames =
      typeParams.map(param => Type.Name(param.name.value))
    lazy val dataType =
      if (typeParams.nonEmpty) t"$name[..$typeParamsNames]" else t"$name"
    lazy val dataPatType =
      if (typeParams.nonEmpty)
        pt"$name[..${Seq.fill(typeParams.length)(pt"_")}]"
      else pt"$name"

    lazy val classParamsTypes = classParams.map(_.decltpe match {
      case Some(tpe: Type) => tpe
      case _               => abort("Currently complicated Type.Args aren't supported")
    })

    lazy val classParamNames =
      classParams.map(param => Term.Name(param.name.value))
    lazy val classParamsWithTypes = classParamNames.zip(classParamsTypes)

    lazy val termName = Term.Name(name.value)

    lazy val dataCreating = q"$termName(..$classParamNames)"

    def getMod(mod: String): Boolean = dataMods.getOrElse(mod, false)
  }
}
