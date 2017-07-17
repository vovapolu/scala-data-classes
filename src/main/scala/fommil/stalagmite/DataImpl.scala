// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package fommil.stalagmite

import fommil.stalagmite.DataInfo.ExtraParams

import scala.collection.immutable.Seq
import scala.meta._

object DataImpl {

  import DataInfo._
  import DataStat._

  def validateClass(ctor: Ctor.Primary,
                    tmpl: Template,
                    tparams: Seq[Type.Param]) = {
    if (ctor.paramss.length > 1) {
      abort("Current implementation doesn't support curried class definitions")
    }

    if (ctor.paramss.flatten.exists(param =>
      param.mods.exists(_ != Mod.ValParam())))
    {
      abort("Fields should be only vals, without modifiers")
    }

    if (ctor.mods.nonEmpty) {
      abort("Current implementation doesn't support constructor params")
    }

    if (tmpl.early.nonEmpty) {
      abort("Current implementation doesn't support early initialization")
    }

    if (tmpl.parents.nonEmpty) {
      abort("Current implementation doesn't support inheritance")
    }

    if (!tmpl.self.name.isInstanceOf[Name.Anonymous]) {
      abort("Current implementation doesn't support self type")
    }

    if (tmpl.stats.map(_.length).getOrElse(0) != 0) {
      abort("Current implementation doesn't support statements in class body")
    }
  }

  def validateDataInfo(dataInfo: DataInfo) = {
    for {
      ref <- dataInfo.extraParams.memoiseRefs
    } {
      if (!dataInfo.classParamNames.map(_.value).contains(ref))
        abort(s"""There's no field called $ref""")
    }
  }

  def buildClass(dataInfo: DataInfo, builders: Seq[DataStatBuilder]): Stat = {
    val ctorParams = dataInfo.classParams.map(param =>
      param"""private[this] var
             ${Term.Name("_" + param.name.value)}: ${param.decltpe.get}""")
    // maybe it will be necessary to create unique names instead of prefix with "_"
    val modsToClasses = Seq(
      "product" -> ctor"_root_.scala.Product",
      "serializable" -> ctor"_root_.scala.Serializable"
    )
    val extendsClasses = modsToClasses.collect {
      case (mod, ctor) if dataInfo.getMod(mod) => ctor
    }

    q"""final class ${dataInfo.name}
          [..${dataInfo.simpleTypeParams}] private (..$ctorParams)
          extends ..${Seq(extendsClasses: _*)} {

       import _root_.scala._
       import _root_.scala.Predef._

       ..${builders.flatMap(_.classStats(dataInfo))}
    }"""
  }

  def buildObject(dataInfo: DataInfo, builders: Seq[DataStatBuilder]): Stat = {
    val modsToClasses = Seq(
      "serializable" -> ctor"_root_.scala.Serializable"
    ) ++ (if (dataInfo.typeParams.isEmpty) {
      Seq("companionExtends" ->
        ctor"((..${dataInfo.classParamsTypes}) => ${dataInfo.dataType})")
    } else {
      Seq()
    })

    val extendsClasses = modsToClasses.collect {
      case (mod, ctor) if dataInfo.getMod(mod) => ctor
    }

    q"""object ${Term.Name(dataInfo.name.value)}
          extends ..${Seq(extendsClasses: _*)} {
       import _root_.scala._
       import _root_.scala.Predef._

       ..${builders.flatMap(_.objectStats(dataInfo))}
    }"""
  }

  def expand(clazz: Defn.Class,
             dataMods: Map[String, Boolean],
             extraParams: ExtraParams): Term.Block = {
    clazz match {
      case cls@Defn.Class(Seq(), name, tparams, ctor, tmpl) =>
        validateClass(ctor, tmpl, tparams)
        val dataInfo = DataInfo(
          name, ctor.paramss.flatten, tparams, dataMods, extraParams
        )
        validateDataInfo(dataInfo)

        val modsToBuilders = Seq(
          "product" -> Seq(DataProductMethodsBuilder),
          "serializable" -> Seq(
            DataWriteObjectBuilder,
            DataReadObjectBuilder,
            DataReadResolveBuilder
          ),
          "shapeless" -> Seq(
            DataShapelessBaseBuilder,
            DataShapelessTypeableBuilder,
            DataShapelessGenericsBuilder
          ),
          "memoise" -> Seq(
            DataMemoiseBuilder
          )
        )

        val builders = Seq(
          DataApplyBuilder,
          DataUnapplyBuilder,
          DataGettersBuilder,
          DataEqualsBuilder,
          DataHashCodeBuilder,
          DataToStringBuilder,
          DataCopyBuilder
        ) ++ modsToBuilders.collect {
          case (mod, bs) if dataInfo.getMod(mod) => bs
        }.flatten

        val newClass = buildClass(dataInfo, builders.toList)
        val newObject = buildObject(dataInfo, builders.toList)
        //println((newClass, newObject).toString())

        Term.Block(Seq(
          newClass,
          newObject
        ))
      case _ =>
        abort("@data must annotate a class without mods.")
    }
  }
}

class data(product: Boolean = false,
           checkSerializable: Boolean = false,
           serializable: Boolean = false,
           shapeless: Boolean = false,
           memoise: Boolean = false,
           memoiseHashCode: Boolean = false,
           memoiseToString: Boolean = false,
           memoiseStrong: Boolean = false,
           memoiseRefs: scala.Seq[scala.Symbol] = scala.Seq())
  extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    val (dataMods, extraParams) = this match {
      case q"new data(..$args)" =>
        //println(args.map(_.structure))
        val mods = args.flatMap {
          case Term.Arg.Named(Term.Name(name), Lit.Boolean(b)) =>
            Some(name -> b)
          case _ =>
            None
        }.toMap[String, Boolean]

        val memoiseRefs = args.collect {
          case Term.Arg.Named(
            Term.Name("memoiseRefs"),
            Term.Apply(Term.Name("Seq"),
            symbols)
          ) => symbols.collect {
            case q"scala.Symbol(${Lit.String(sym)})" => sym
          }
        }.headOption.getOrElse(Seq())

        (mods, ExtraParams(memoiseRefs))
      case _ => (Map.empty[String, Boolean], ExtraParams())
    }

    defn match {
      case Term.Block(Seq(
        cls@Defn.Class(Seq(), name, Seq(), ctor, tmpl),
        companion: Defn.Object
      )) =>
        abort("@data block")
      case clazz: Defn.Class => DataImpl.expand(clazz, dataMods, extraParams)
    }
  }
}
