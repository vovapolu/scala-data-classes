package fommil.data.impl

import scala.collection.immutable.Seq
import scala.meta._

object DataImpl {

  import DataInfo._
  import DataStat._

  def validateCtor(ctor: Ctor.Primary) = {
    if (ctor.paramss.length > 1) {
      abort("Current implementation doesn't support curried class definitions")
    }

    if (ctor.paramss.flatten.exists(param => param.mods.exists(_ != Mod.ValParam()))) {
      abort("Invalid constructor!")
    }
  }

  def extractDataInfo(name: Type.Name,
                                         ctor: Ctor.Primary,
                                         tparams: Seq[Type.Param],
                                         dataMods: DataMods): DataInfo = {
    validateCtor(ctor)
    DataInfo(name, ctor.paramss.flatten, tparams, dataMods)
  }

  def buildClass(dataInfo: DataInfo, builders: Seq[DataStatBuilder]): Stat = {
    val ctorParams = dataInfo.classParams.map(param =>
      param"private[this] var ${Term.Name("_" + param.name.value)}: ${param.decltpe.get}")
    // maybe it will be necessary to create unique names instead of prefix with "_"

    q"""final class ${dataInfo.name}[..${dataInfo.simpleTypeParams}] private (..$ctorParams)
                         extends Product with Serializable {
       ..${builders.flatMap(_.classStats(dataInfo))}
    }"""
  }

  def buildObject(dataInfo: DataInfo, builders: Seq[DataStatBuilder]): Stat = {
    q"""object ${Term.Name(dataInfo.name.value)} {
       ..${builders.flatMap(_.objectStats(dataInfo))}
    }"""
  }

  //  private [dataMacro] def getUsedNames(stats: Seq[Stat]): Seq[String] = {
  //    stats.flatMap {
  //      case q"..$_ val ..$names: $_ = $_" => names.map(name => name.name.value)
  //      case q"..$_ var ..$names: $_ = $_" => names.map(name => name.name.value)
  //      case q"..$_ def $name[..$_](...$_): $_ = $_" => Seq(name.value)
  //      case q"..$_ type $tname[..$_] = $_" => Seq(tname.value)
  //      case q"..$_ class $tname[..$_] ..$_ (...$_) extends $_" => Seq(tname.value)
  //      case q"..$_ trait $tname[..$_] extends $_" => Seq(tname.value)
  //      case q"..$_ object $name extends $_" => Seq(name.value)
  //      case q"..$_ val ..$names: $_" => names.map(name => name.name.value)
  //      case q"..$_ var ..$names: $_" => names.map(name => name.name.value)
  //      case q"..$_ def $name[..$_](...$_): $tpe" => Seq(name.value)
  //      case q"..$_ type $tname[..$_] >: $_ <: $_" => Seq(tname.value)
  //      case _ => Seq()
  //    }
  //  }

//  private[dataMacro] def validateUsedNames(names: Seq[String], dataMods: DataMods) = {
//    val methodAbort = (method: String) => abort(s"""Data class shouldn't contain "$method" method""")
//    names.foreach {
//      case "apply" => methodAbort("apply")
//      case "unapply" => methodAbort("unapply")
//      case "productArity" => methodAbort("productArity")
//      case "productElement" => methodAbort("productElement")
//      case "productPrefix" => methodAbort("productPrefix")
//      case "productIterator" => methodAbort("productIterator")
//      case "copy" => methodAbort("copy")
//
//      case "equals" if dataMods.idEquals || dataMods.intern =>
//        abort(s"""Data class shouldn't contain "equals" method with modificators""")
//      case "equals" if dataMods.idEquals || dataMods.intern =>
//        abort(s"""Data class shouldn't contain "equals" method with modificators""")
//    }
//  }

  def expand(clazz: Defn.Class,
             intern: Boolean = false,
             idEquals: Boolean = false): Term.Block = {
    clazz match {
      case cls@Defn.Class(Seq(), name, tparams, ctor, tmpl) =>
        val dataInfo = extractDataInfo(name, ctor, tparams, DataMods(intern, idEquals))
        val builders = Seq(
          DataApplyBuilder,
          DataUnapplyBuilder,
          DataGettersBuilder,
          DataEqualsBuilder,
          DataHashCodeBuilder,
          DataToStringBuilder,
          DataWriteObjectBuilder,
          DataReadObjectBuilder,
          DataReadResolveBuilder,
          DataCopyBuilder,
          DataProductMethodsBuilder)
        val newClass = buildClass(dataInfo, builders)
        val newObject = buildObject(dataInfo, builders)
        println((newClass, newObject).toString())

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
           intern: Boolean = false,
           idEquals: Boolean = false) extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    //println(this.structure)
    var intern: Boolean = false
    var idEquals: Boolean = false
    this match {
      case q"new data(..$args)" => args.foreach {
        case arg"intern = ${Lit(b: Boolean)}" => intern = b
        case arg"idEquals = ${Lit(b: Boolean)}" => idEquals = b
        case _ =>
      }
      case _ =>
    }

    if (intern && idEquals) {
      abort("Can't do interning with id equations")
    }

    defn match {
      case Term.Block(Seq(cls@Defn.Class(Seq(), name, Seq(), ctor, tmpl), companion: Defn.Object)) =>
        abort("@data block")
      case clazz: Defn.Class => DataImpl.expand(clazz, intern, idEquals)
    }
  }
}
