// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package fommil.stalagmite.stats

import fommil.stalagmite.DataInfo
import fommil.stalagmite.DataStats

import scala.collection.immutable.Seq
import scala.meta._

object ShapelessStats {

  object DataShapelessBaseStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {

      val typeSymbols = dataInfo.classParamNames.map(
        name => q"""val ${Pat.Var.Term(Term.Name(name.value + "_tpe"))} =
            Symbol(${Lit.String(name.value)}).narrow"""
      )

      Seq(
        q"""import _root_.shapeless.{
           ::, HNil, Generic, LabelledGeneric, Typeable}""",
        q"import _root_.shapeless.labelled.{FieldType, field}",
        q"import _root_.shapeless.syntax.singleton._"
      ) ++ typeSymbols
    }
  }

  object DataShapelessTypeableStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val typeableName = Term.Name("Typeable" + dataInfo.name.value)

      def isTypeParamOrHasTypeParams(tpe: Type) =
        tpe match {
          case tname: Type.Name =>
            dataInfo.typeParams.map(_.name.value).contains(tname.value)
          case _ =>
            true // there are more cases, but it handles basic types at least
        }

      val distinctClassParamsTypes = Seq(
        dataInfo.classParamsTypes
          .groupBy(_.toString())
          .values
          .collect { case head :: _ => head }
          .toList
          .filter(isTypeParamOrHasTypeParams): _*
      )
      val typesToTypeCasesNames =
        distinctClassParamsTypes.zipWithIndex.map {
          case (tpe, ind) => (tpe.toString(), Term.Name(s"TC$ind"))
        }.toMap
      val typeCases = distinctClassParamsTypes
        .map(
          tpe => q"""val ${Pat.Var.Term(typesToTypeCasesNames(s"$tpe"))}
               = TypeCase[$tpe]"""
        )

      val typesToTypeableNames =
        distinctClassParamsTypes.zipWithIndex.map {
          case (tpe, ind) => (tpe.toString(), Term.Name(s"T$ind"))
        }.toMap
      val implicitTypeables = distinctClassParamsTypes
        .map(
          tpe =>
            param"implicit ${typesToTypeableNames(s"$tpe")}: Typeable[$tpe]"
        )

      val typeCasesWithImport = if (typeCases.nonEmpty) {
        q"import _root_.shapeless.TypeCase" +: typeCases
      } else {
        typeCases
      }
      val dataWithTypeCases = {
        val args = dataInfo.classParamsWithTypes.map {
          case (param, tpe) =>
            if (isTypeParamOrHasTypeParams(tpe)) {
              p"${typesToTypeCasesNames(s"$tpe")}(${Pat.Var.Term(param)})"
            } else {
              p"${Pat.Var.Term(param)}"
            }
        }
        p"${dataInfo.termName}(..$args)"
      }

      val describe = {
        def getTypeDescribe(tpe: Type): Term =
          if (isTypeParamOrHasTypeParams(tpe)) {
            q"${typesToTypeableNames(s"$tpe")}.describe"
          } else {
            Lit.String(tpe.toString())
          }

        val targs = {
          dataInfo.classParamsTypes match {
            case Seq(t1) => getTypeDescribe(t1)
            case Seq(t1, rest @ _*) =>
              rest.foldLeft[Term](getTypeDescribe(t1)) {
                case (expr, tname) =>
                  q"$expr + ${Lit.String(",")} + ${getTypeDescribe(tname)}"
              }
          }
        }
        q"""${Lit.String(dataInfo.name.value + "[")} +
           $targs + ${Lit.String("]")}"""
      }

      val typeableBody =
        q"""
           new Typeable[${dataInfo.dataType}] {
             override def cast(t: Any): Option[${dataInfo.dataType}] = {
               ..$typeCasesWithImport
               t match {
                 case f @ $dataWithTypeCases => Some(${dataInfo.dataCreating})
                 case _                      => None
               }
             }
             override def describe: String = $describe
           }
          """

      if (implicitTypeables.nonEmpty) {
        Seq(q"""
          implicit def $typeableName[..${dataInfo.simpleTypeParams}](
              ..$implicitTypeables
            ): Typeable[${dataInfo.dataType}] = $typeableBody
          """)
      } else {
        Seq(
          q"""implicit def $typeableName
             [..${dataInfo.simpleTypeParams}]: Typeable[${dataInfo.dataType}] =
            $typeableBody"""
        )
      }
    }
  }

  object DataShapelessGenericsStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val genericName = Term.Name("Generic" + dataInfo.name.value)
      val genericWithTypes = if (dataInfo.simpleTypeParams.nonEmpty) {
        q"$genericName[..${dataInfo.typeParamsNames}]"
      } else {
        genericName
      }
      val labelledGenericName =
        Term.Name("LabelledGeneric" + dataInfo.name.value)
      val labelledGenericWithTypes = if (dataInfo.typeParamsNames.nonEmpty) {
        q"$labelledGenericName[..${dataInfo.typeParamsNames}]"
      } else {
        labelledGenericName
      }

      val reprType =
        dataInfo.classParamsTypes.foldRight[Type](Type.Name("HNil")) {
          case (tpe, expr) => t"$tpe :: $expr"
        }
      val labelledReprType =
        dataInfo.classParamsWithTypes.foldRight[Type](Type.Name("HNil")) {
          case ((param, tpe), expr) =>
            t"FieldType[${Term.Name(param.value + "_tpe")}.type, $tpe] :: $expr"
        }

      val labelledFields =
        dataInfo.classParamNames.foldRight[Term](Term.Name("HNil")) {
          case (param, expr) =>
            q"field[${Term.Name(param.value + "_tpe")}.type](f.$param) :: $expr"
        }
      val fields = dataInfo.classParamNames.foldRight[Pat](Term.Name("HNil")) {
        case (param, expr) => p"${Pat.Var.Term(param)} :: $expr"
      }

      val generic =
        q"""
        implicit def $genericName[..${dataInfo.simpleTypeParams}]:
          Generic.Aux[${dataInfo.dataType}, $reprType] =
          new Generic[${dataInfo.dataType}] {
            override type Repr = $reprType
            override def to(f: ${dataInfo.dataType}): Repr =
              $labelledGenericWithTypes.to(f)
            override def from(r: Repr): ${dataInfo.dataType} = r match {
              case ($fields) => ${dataInfo.dataCreating}
            }
        }"""

      val labelledGeneric =
        q"""
        implicit def $labelledGenericName[..${dataInfo.simpleTypeParams}]:
          LabelledGeneric.Aux[${dataInfo.dataType}, $labelledReprType] =
          new LabelledGeneric[${dataInfo.dataType}] {
            override type Repr = $labelledReprType
            override def to(f: ${dataInfo.dataType}): Repr = $labelledFields
            override def from(r: Repr): ${dataInfo.dataType} =
              $genericWithTypes.from(r)
          }
          """

      Seq(generic, labelledGeneric)
    }
  }
}
