// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package fommil.stalagmite

import scala.collection.immutable.Seq
import scala.meta._

object DataImpl {

  import stats.CaseClassStats._
  import stats.MemoisationStats._
  import stats.SerializableStats._
  import stats.ShapelessStats._
  import stats.HeapOptimizationStats._

  def validateClass(ctor: Ctor.Primary,
                    tmpl: Template,
                    tparams: Seq[Type.Param]) = {
    if (ctor.paramss.length > 1) {
      abort("Current implementation doesn't support curried class definitions")
    }

    if (ctor.paramss.flatten.exists(
          param => param.mods.exists(_ != Mod.ValParam())
        )) {
      abort("Fields should be only vals, without modifiers")
    }

    if (ctor.paramss.flatten.isEmpty) {
      abort("Class shouldn't be empty")
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

    tmpl.self.name match {
      case _: Name.Anonymous =>
      case _                 => abort("Current implementation doesn't support self type")
    }

    if (tmpl.stats.map(_.length).getOrElse(0) != 0) {
      abort("Current implementation doesn't support statements in class body")
    }
  }

  def validateDataInfo(dataInfo: DataInfo) = {
    for {
      ref <- dataInfo.dataMods.memoiseRefs
    } {
      if (!dataInfo.classParamNames.map(_.value).contains(ref))
        abort(s"""There's no field called $ref""")
    }

    if (dataInfo.requiredToPack && dataInfo.dataMods.memoise) {
      abort("Heap optimization and memoisation cannot by applied together")
    }
  }

  def buildClass(dataInfo: DataInfo, builders: Seq[DataStats]): Stat = {
    val actualFields = if (dataInfo.requiredToPack) {
      dataInfo.optimizedParams
    } else {
      dataInfo.classParamsWithTypes
    }

    def generateField(field: (Term.Name, Type)): Term.Param = {
      val (param, tpe) = field
      if (dataInfo.requiresToHaveVars) {
        param"private[this] var ${Term.Name("_" + param.value)}: $tpe"
      } else {
        param"private[this] val ${Term.Name("_" + param.value)}: $tpe"
      }
    }

    val ctorParams = actualFields.map(generateField) ++
      (if (dataInfo.dataMods.weakMemoisation) {
         Seq(
           param"@_root_.scala.transient private val _key: ${MemoisationStatsHelper.keyType(dataInfo)}"
         )
       } else {
         Seq.empty
       })

    val modsToClasses: Seq[(DataInfo => Boolean, Ctor.Call)] = Seq(
      ((di: DataInfo) => di.dataMods.product) ->
        ctor"_root_.scala.Product",
      ((di: DataInfo) => di.dataMods.serializable) ->
        ctor"_root_.scala.Serializable"
    )
    val extendsClasses = modsToClasses.collect {
      case (dataInfoP, ctor) if dataInfoP(dataInfo) => ctor
    }

    q"""final class ${dataInfo.name}
          [..${dataInfo.simpleTypeParams}] private (..$ctorParams)
          extends ..${Seq(extendsClasses: _*)} {

       ..${builders.flatMap(_.classStats(dataInfo))}
    }"""
  }

  def buildObject(dataInfo: DataInfo, builders: Seq[DataStats]): Stat = {
    val modsToClasses: Seq[(DataInfo => Boolean, Ctor.Call)] = Seq(
      ((di: DataInfo) => di.dataMods.serializable) ->
        ctor"_root_.scala.Serializable"
    ) ++ (if (dataInfo.typeParams.isEmpty) {
            Seq(
              ((di: DataInfo) => di.dataMods.companionExtends) ->
                ctor"((..${dataInfo.classParamsTypes}) => ${dataInfo.dataType})"
            )
          } else {
            Seq()
          })

    val extendsClasses = modsToClasses.collect {
      case (dataInfoP, ctor) if dataInfoP(dataInfo) => ctor
    }

    q"""object ${Term.Name(dataInfo.name.value)}
          extends ..${Seq(extendsClasses: _*)} {

       ..${builders.flatMap(_.objectStats(dataInfo))}
    }"""
  }

  def composeBuilderByTags(builders: Seq[DataStats]): Seq[DataStats] = {

    // keep only first builder for each tag
    def filterBuilders(unfilteredBuilders: Seq[DataStats],
                       usedTags: Set[DataStats.StatsTag]): Seq[DataStats] =
      unfilteredBuilders match {
        case head +: tail =>
          head.statsTag match {
            case DataStats.NoOverride =>
              head +: filterBuilders(tail, usedTags)
            case tag if !usedTags.contains(tag) =>
              head +: filterBuilders(tail, usedTags + tag)
            case _ =>
              filterBuilders(tail, usedTags)
          }
        case _ => Seq.empty
      }

    filterBuilders(builders.reverse, Set.empty).reverse
  }

  def expand(clazz: Defn.Class, dataMods: DataMods): Term.Block =
    clazz match {
      case cls @ Defn.Class(Seq(), name, tparams, ctor, tmpl) =>
        validateClass(ctor, tmpl, tparams)
        val dataInfo = DataInfo(
          name,
          ctor.paramss.flatten,
          tparams,
          dataMods
        )
        validateDataInfo(dataInfo)

        val modsToStats: Seq[(DataInfo => Boolean, Seq[DataStats])] = Seq(
          ((di: DataInfo) => di.dataMods.product) -> Seq(
            DataProductMethodsStats
          ),
          ((di: DataInfo) => di.dataMods.serializable) -> Seq(
            DataWriteObjectStats,
            DataReadObjectStats,
            DataReadResolveStats
          ),
          ((di: DataInfo) => di.dataMods.shapeless) -> Seq(
            DataShapelessBaseStats,
            DataShapelessTypeableStats,
            DataShapelessGenericsStats
          ),
          ((di: DataInfo) => di.dataMods.strongMemoisation) -> Seq(
            DataStrongMemoiseApplyStats,
            DataStrongMemoiseStats
          ),
          ((di: DataInfo) => di.dataMods.weakMemoisation) -> Seq(
            DataWeakMemoiseApplyStats,
            DataWeakMemoiseStats
          ),
          ((di: DataInfo) => di.requiredToPack) -> Seq(
            UnpackGettersStats,
            DataHeapOptimizeApplyStats,
            PackStats
          )
        )

        val builders = Seq(
          DataApplyStats,
          DataUnapplyStats,
          DataGettersStats,
          DataEqualsStats,
          DataHashCodeStats,
          DataToStringStats,
          DataCopyStats
        ) ++ modsToStats.collect {
          case (dataInfoP, bs) if dataInfoP(dataInfo) => bs
        }.flatten.toList

        val newClass  = buildClass(dataInfo, composeBuilderByTags(builders))
        val newObject = buildObject(dataInfo, composeBuilderByTags(builders))
        //println((newClass, newObject).toString())

        Term.Block(
          Seq(
            newClass,
            newObject
          )
        )
      case _ =>
        abort("@data must annotate a class without mods.")
    }
}

/**
 * Data annotation
 *
 * @param product Adds methods from Product trait:
 *                `productElement`, `productArity`, `productPrefix`, `productIterator`.
 *                Default value is `false`.
 *
 * @param checkSerializable Adds in compile time validation
 *                          that all class fields are `Serializable`
 *                          Default value is `true`.
 *
 * @param companionExtends Make companion inherit corresponding `Function` class.
 *                         Default value is `false`.
 *
 * @param serializable Adds support for Serialization:
 *                     `readObject`, `writeObject` methods if necessary,
 *                     `readResolve`.
 *                     Default value is `true`.
 *
 * @param shapeless Adds shapeless helper objects:
 *                  `Typeable`, `Generic`, `LabeledGeneric`.
 *                  Default value is `true`.
 *
 * @param memoise Adds memoisation mechanism to the class.
 *                If `memoiseStrong` is false the weak memosation is applied.
 *                Weak memoisation currently removes `.equals` method at all.
 *                See `memoiseEqualsByValue`.
 *                Default value is `false`.
 *
 * @param memoiseStrong Applies strong memoisation instead of weak.
 *                      Doesn't work without `memoise` flag.
 *                      Default value is `false`.
 *                      Default value is `false`.
 *
 * @param memoiseEqualsByValue If weak memoisation is used
 *                             then adds regular `.equals` by value.
 *                             Default value is `true`.
 *
 * @param memoiseHashCode Caches `.hashCode` to `val`
 *                        Default value is `false`.
 *
 * @param memoiseHashCodeLazy Caches `.hashCode` to `lazy val`
 *                            Default value is `false`.
 *
 * @param memoiseToString Caches `.toString` to `val`
 *                        Default value is `false`.
 *
 * @param memoiseToStringLazy Caches `.toString` to `lazy val`
 *                            Default value is `false`.
 *
 * @param memoiseRefs List of names that represent fields memoised in separate cache
 *                    Default value is `false`.
 *
 * @param optimiseHeapOptions Packs options.
 *                            If `Option` wraps primitive value,
 *                            `Option` is packed into bitmask and
 *                            inner primitive value becomes class fields.
 *                            If `Option` wraps `AnyRef` value,
 *                            inner `AnyRef` becomes nullable class field with
 *                            `null` for the `None` case.
 *                            Default value is `false`.
 *
 * @param optimiseHeapBooleans Packs `Boolean` fields into bitmask.
 *                             With `optimiseHeapOptions` packs inner `Boolean`
 *                             in case of `Option[Boolean]` fields
 *                             Default value is `false`.
 *
 * @param optimiseHeapStrings Packs `String` fields to `Array[Byte]`.
 *                            With `optimiseHeapOptions` packs `Option[String]`.
 *                            Default value is `false`.
 */
class data(product: Boolean = false,
           checkSerializable: Boolean = true,
           companionExtends: Boolean = false,
           serializable: Boolean = true,
           shapeless: Boolean = true,
           // memoise mods
           memoise: Boolean = false,
           memoiseStrong: Boolean = false,
           memoiseEqualsByValue: Boolean = true,
           memoiseHashCode: Boolean = false,
           memoiseHashCodeLazy: Boolean = false,
           memoiseToString: Boolean = false,
           memoiseToStringLazy: Boolean = false,
           memoiseRefs: scala.Seq[scala.Symbol] = scala.Seq(),
           // optimize heap mods
           optimiseHeapOptions: Boolean = false,
           optimiseHeapBooleans: Boolean = false,
           optimiseHeapStrings: Boolean = false)
    extends scala.annotation.StaticAnnotation {

  // workaround for https://github.com/scalameta/scalameta/issues/1038
  val dummy = 'dummy

  inline def apply(defn: Any): Any = meta {

    val dataMods = this match {
      case q"new data(..$args)" =>
        val pairs = args.collect {
          case Term.Arg.Named(Term.Name(name), Lit.Boolean(b)) =>
            name -> Left(b)
          case Term.Arg.Named(
              Term.Name("memoiseRefs"),
              Term.Apply(Term.Name("Seq"), symbols)
              ) =>
            "memoiseRefs" -> Right(symbols.collect {
              case q"scala.Symbol(${Lit.String(sym) })" => sym
            })
        }
        DataMods.fromPairs(pairs)

      case _ => DataMods()
    }

    defn match {
      case Term.Block(
          Seq(cls @ Defn.Class(Seq(), name, Seq(), ctor, tmpl),
              companion: Defn.Object)
          ) =>
        abort("@data block")
      case clazz: Defn.Class => DataImpl.expand(clazz, dataMods)
    }
  }
}
