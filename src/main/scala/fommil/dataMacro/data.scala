package fommil.dataMacro

import fommil.dataMacro.DataMacro.DataMods

import scala.collection.immutable.Seq
import scala.meta._

object DataMacro {
  private[dataMacro] case class DataParam(name: String, tpe: Type, default: Option[Term])
  private[dataMacro] case class DataMods(intern: Boolean, idEquals: Boolean)

  private[dataMacro] def validateCtor(ctor: Ctor.Primary) = {
    if (ctor.paramss.length > 1) {
      abort("Current implementation doesn't support curried class definitions")
    }

    if (ctor.paramss.flatten.exists(param => param.mods.exists(_ != Mod.ValParam()))) {
      abort("Invalid constructor!")
    }
  }

  private[dataMacro] def extractDataParams(ctor: Ctor.Primary): Seq[DataParam] = {
    validateCtor(ctor)
    ctor.paramss.flatten.map {
      case Term.Param(_, name, Some(tpe: Type), default) => DataParam(name.value, tpe, default)
      case _ => abort("Invalid constructor!")
    }
  }

  private[dataMacro] def buildCtorParams(dataParams: Seq[DataParam]): Seq[Term.Param] = {
    dataParams.map(
      param => Term.Param(Seq(Mod.ValParam()), Term.Name(param.name), Some(param.tpe), param.default))
  }

  private[dataMacro] def extractSimpleTypeParams(tparams: Seq[Type.Param]): Seq[Type.Param] = {
    tparams.map(tparam => tparam.copy(mods = Seq(), tbounds = Type.Bounds(None, None)))
  }

  private[dataMacro] def buildApply(name: Type.Name,
                                    dataParams: Seq[DataParam],
                                    tparams: Seq[Type.Param],
                                    intern: Boolean): Defn.Def = {
    val args = dataParams.map(param => Term.Name(param.name))
    val params = dataParams.map(param => param"${Term.Name(param.name)}: ${param.tpe}")
    val simpleTparams = extractSimpleTypeParams(tparams)
    val tparamsNames = simpleTparams.map(param => Type.Name(param.name.value))
    val classType = if (tparamsNames.nonEmpty) t"$name[..$tparamsNames]" else t"$name"
    if (intern) {
      q"""def apply[..$simpleTparams](..$params): $classType = {
          val newVal = new ${Ctor.Ref.Name(name.value)}(..$args)
          ${Term.Name(name.value)}.intern(newVal)
        }"""
    } else {
      q"""def apply[..$simpleTparams](..$params): $classType =
        new ${Ctor.Ref.Name(name.value)}(..$args)"""
    }
  }

  private[dataMacro] def buildUnapply(name: Type.Name,
                                      dataParams: Seq[DataParam],
                                      tparams: Seq[Type.Param]): Defn.Def = {
    val clFields = dataParams.map(param => q"that.${Term.Name(param.name)}")
    val simpleTparams = extractSimpleTypeParams(tparams)
    val tparamsNames = simpleTparams.map(param => Type.Name(param.name.value))
    val classType = if (tparamsNames.nonEmpty) t"$name[..$tparamsNames]" else t"$name"

    val tupleTypes = if (dataParams.length > 1) {
      t"(..${dataParams.map(_.tpe)})"
    } else {
      t"${dataParams.head.tpe}"
    }
    val tupleArgs = if (clFields.length > 1) {
      q"(..$clFields)"
    } else {
      clFields.head
    }
    q"def unapply[..$simpleTparams](that: $classType): Option[$tupleTypes] = Some($tupleArgs)"
  }

  private[dataMacro] def buildEquals(name: Type.Name,
                                     dataParams: Seq[DataParam],
                                     mods: DataMods,
                                     tparams: Seq[Type.Param]): Defn.Def = {
    val eqs = dataParams.map(param => q"that.${Term.Name(param.name)} == this.${Term.Name(param.name)}")
    val eqsWithAnds = eqs match {
      case Seq(eq1) => eq1
      case Seq(eq1, rest @ _ *) => rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
    }

    val simpleTparams = extractSimpleTypeParams(tparams)
    val tparamsNames = simpleTparams.map(param => Type.Name(param.name.value))
    val classType = if (tparamsNames.nonEmpty) pt"$name[..$tparamsNames]" else pt"$name"

    if (mods.intern) {
      q"override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object])"
    } else if (mods.idEquals) {
      q"""override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
      (thatAny match {
        case that: $classType =>
          val idTuple = if (this.id > that.id) (that.id, this.id) else (this.id, that.id)
          val res = ${Term.Name(name.value)}.idEquals.get(idTuple)
          if (res == null) {
            val realEquals = $eqsWithAnds
            ${Term.Name(name.value)}.idEquals.put(idTuple, Boolean.box(realEquals))
            realEquals
          } else {
            Boolean.unbox(res)
          }
        case _ => false
     })"""
    } else {
      q"""override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
      (thatAny match {
        case that: $classType => $eqsWithAnds
        case _ => false
     })"""
    }
  }

  private[dataMacro] def buildProductMethods(name: Type.Name,
                                             dataParams: Seq[DataParam],
                                             tparams: Seq[Type.Param]): Seq[Defn.Def] = {
    val casesForElements = dataParams.zipWithIndex.map {
      case (param, i) => p"case ${Lit.Int(i)} => this.${Term.Name(param.name)}"
    }
    val simpleTparams = extractSimpleTypeParams(tparams)
    val tparamsNames = simpleTparams.map(param => Type.Name(param.name.value))
    val classType = if (tparamsNames.nonEmpty) t"$name[..$tparamsNames]" else t"$name"
    Seq(
      q"def canEqual(that: Any): Boolean = that.isInstanceOf[$classType]",
      q"def productArity: Int = ${Lit.Int(dataParams.length)}",
      q"""def productElement(n: Int): Any =
         n match {
            ..case $casesForElements
            case _ => throw new IndexOutOfBoundsException(n.toString())
         }
       """,
      q"override def productPrefix: String = ${Lit.String(name.value)}",
      q"override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)"
    )
  }

  private[dataMacro] def buildHashCode(name: Type.Name, dataParams: Seq[DataParam]): Defn.Def = {
    q"override def hashCode(): Int = scala.runtime.ScalaRunTime._hashCode(this)"
  }

  private[dataMacro] def buildToString(name: Type.Name, dataParams: Seq[DataParam]): Defn.Def = {
    q"override def toString(): String = scala.runtime.ScalaRunTime._toString(this)"
  }

  private[dataMacro] def buildCopy(name: Type.Name,
                                   dataParams: Seq[DataParam],
                                   tparams: Seq[Type.Param]): Defn.Def = {
    val copyParams = dataParams.map(
      param => Term.Param(Seq(), Term.Name(param.name), Some(param.tpe), Some(q"this.${Term.Name(param.name)}")))
    val copyArgs = dataParams.map(param => Term.Name(param.name))
    val simpleTparams = extractSimpleTypeParams(tparams)
    val tparamsNames = simpleTparams.map(param => Type.Name(param.name.value))
    val classType = if (tparamsNames.nonEmpty) t"$name[..$tparamsNames]" else t"$name"
    q"def copy(..$copyParams): $classType = new ${Ctor.Ref.Name(name.value)}(..$copyArgs)"
  }

  private[dataMacro] def buildInternMap(name: Type.Name, dataParams: Seq[DataParam]): Seq[Stat] = {
    val wrapperName = Type.Name(name.value + "Wrapper")
    val eqs = dataParams.map(param => q"that.cl.${Term.Name(param.name)} == this.cl.${Term.Name(param.name)}")
    val eqsWithAnds = eqs match {
      case Seq(eq1) => eq1
      case Seq(eq1, rest @ _ *) => rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
    }
    Seq(
      q"""private[$name] class $wrapperName (val cl: $name) {
          override def hashCode(): Int = cl.hashCode
          override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
            (thatAny match {
              case that: $wrapperName => $eqsWithAnds
              case _ => false
            })
       }""",
      q"""private[$name] val internMap: java.util.concurrent.ConcurrentHashMap[$wrapperName, $name] =
         new java.util.concurrent.ConcurrentHashMap()""",
      q"""def intern(cl: $name): $name = {
          val res = internMap.putIfAbsent(new ${Ctor.Ref.Name(wrapperName.value)}(cl), cl)
          if (res == null) cl else res
      }"""
    )
  }

  private[dataMacro] def buildIntern(name: Type.Name): Defn.Def = {
    q"def intern: $name = ${Term.Name(name.value)}.intern(this)"
  }

  private[dataMacro] def buildIdGenerator(name: Type.Name): Seq[Defn.Val] = {
    Seq(
      q"private[$name] val idGenerator = new java.util.concurrent.atomic.AtomicInteger()",
      q"private[$name] val idEquals = new java.util.concurrent.ConcurrentHashMap[(Int, Int), java.lang.Boolean]()"
    )
  }

  private[dataMacro] def buildIdVal(name: Type.Name): Defn.Val = {
    q"private[$name] val id: Int = ${Term.Name(name.value)}.idGenerator.incrementAndGet()"
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

  private [dataMacro] def validateUsedNames(names: Seq[String], dataMods: DataMods) = {
    val methodAbort = (method: String) => abort(s"""Data class shouldn't contain "$method" method""")
    names.foreach {
      case "apply" => methodAbort("apply")
      case "unapply" => methodAbort("unapply")
      case "productArity" => methodAbort("productArity")
      case "productElement" => methodAbort("productElement")
      case "productPrefix" => methodAbort("productPrefix")
      case "productIterator" => methodAbort("productIterator")
      case "copy" => methodAbort("copy")

      case "equals" if dataMods.idEquals || dataMods.intern =>
        abort(s"""Data class shouldn't contain "equals" method with modificators""")
      case "equals" if dataMods.idEquals || dataMods.intern =>
        abort(s"""Data class shouldn't contain "equals" method with modificators""")
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
      case cls@Defn.Class(Seq(), name, tparams, ctor, tmpl) =>
        tmpl.stats
        val dataParams = DataMacro.extractDataParams(ctor)
        val simpleTparams = DataMacro.extractSimpleTypeParams(tparams)
        val newClass: Stat = q"""class ${Type.Name(name.value)}[..$simpleTparams] (..${DataMacro.buildCtorParams(dataParams)})
                         extends Product with Serializable {
          ${DataMacro.buildEquals(name, dataParams, DataMods(intern, idEquals), tparams)}
          ${DataMacro.buildToString(name, dataParams)}
          ${DataMacro.buildHashCode(name, dataParams)}
          ${DataMacro.buildCopy(name, dataParams, tparams)}
          ..${DataMacro.buildProductMethods(name, dataParams, tparams)}
          ..${if (intern) Seq(DataMacro.buildIntern(name)) else Seq()}
          ..${if (idEquals) Seq(DataMacro.buildIdVal(name)) else Seq()}
        }"""
        val newObject: Stat = q"""object ${Term.Name(name.value)} {
          ${DataMacro.buildApply(name, dataParams, tparams, intern)}
          ${DataMacro.buildUnapply(name, dataParams, tparams)}
          ..${if (intern) DataMacro.buildInternMap(name, dataParams) else Seq()}
          ..${if (idEquals) DataMacro.buildIdGenerator(name) else Seq()}
        }"""

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
