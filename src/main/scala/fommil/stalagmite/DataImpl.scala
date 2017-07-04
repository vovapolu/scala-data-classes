package fommil.stalagmite

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
                      dataMods: Map[String, Boolean]): DataInfo = {
    validateCtor(ctor)
    DataInfo(name, ctor.paramss.flatten, tparams, dataMods)
  }

  def buildClass(dataInfo: DataInfo, builders: Seq[DataStatBuilder]): Stat = {
    val ctorParams = dataInfo.classParams.map(param =>
      param"private[this] var ${Term.Name("_" + param.name.value)}: ${param.decltpe.get}")
    // maybe it will be necessary to create unique names instead of prefix with "_"
    val modsToClasses = Seq(
      "product" -> ctor"_root_.scala.Product",
      "serializable" -> ctor"_root_.scala.Serializable"
    )
    val extendsClasses = modsToClasses.collect {
      case (mod, ctor) if dataInfo.getMod(mod) => ctor
    }

    q"""final class ${dataInfo.name}[..${dataInfo.simpleTypeParams}] private (..$ctorParams)
                         extends ..${Seq(extendsClasses: _*)} {
       import _root_.scala._
       import _root_.scala.Predef._

       ..${builders.flatMap(_.classStats(dataInfo))}
    }"""
  }

  def buildObject(dataInfo: DataInfo, builders: Seq[DataStatBuilder]): Stat = {
    val modsToClasses = Seq(
      "serializable" -> ctor"_root_.scala.Serializable"
    )
    val extendsClasses = modsToClasses.collect {
      case (mod, ctor) if dataInfo.getMod(mod) => ctor
    }

    q"""object ${Term.Name(dataInfo.name.value)} extends ..${Seq(extendsClasses: _*)} {
       import _root_.scala._
       import _root_.scala.Predef._

       ..${builders.flatMap(_.objectStats(dataInfo))}
    }"""
  }

  def expand(clazz: Defn.Class,
             dataMods: Map[String, Boolean]): Term.Block = {
    clazz match {
      case cls@Defn.Class(Seq(), name, tparams, ctor, tmpl) =>
        val dataInfo = extractDataInfo(name, ctor, tparams, dataMods)

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

class data(product: Boolean,
           serializable: Boolean,
           shapeless: Boolean) extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    println(this.structure)

    val dataMods: Map[String, Boolean] = this match {
      case q"new data(..$args)" => args.flatMap {
        case Term.Arg.Named(Term.Name(name), Lit.Boolean(b)) => Some(name -> b)
        case _ => None
      }.toMap
      case _ => Map.empty
    }

    println(dataMods)

    defn match {
      case Term.Block(Seq(cls@Defn.Class(Seq(), name, Seq(), ctor, tmpl), companion: Defn.Object)) =>
        abort("@data block")
      case clazz: Defn.Class => DataImpl.expand(clazz, dataMods)
    }
  }
}
