// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package fommil.stalagmite.stats

import fommil.stalagmite.DataInfo
import fommil.stalagmite.DataStats

import scala.collection.immutable.Seq
import scala.meta._

object HeapOptimizationStats {

  object DataHeapOptimizeApplyStats extends DataStats {
    import CaseClassStats.DataApplyStats

    override val statsTag: DataStats.StatsTag = DataStats.ApplyStats

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val packedArgs = dataInfo.optimizedParams match {
        case Seq(param1) => Seq(q"packed")
        case params =>
          params.indices.map(ind => q"packed.${Term.Name(s"_${ind + 1}")}")
      }

      Seq(
        q"""def apply[..${dataInfo.simpleTypeParams}](
            ..${DataApplyStats.applyParams(dataInfo)}
          ): ${dataInfo.dataType} = {
          val packed = pack(..${dataInfo.classParamNames})
          val created = new ${Ctor.Ref.Name(dataInfo.name.value)}(
            ..$packedArgs
            )
          created.synchronized(created)
        }"""
      )
    }
  }

  /**
   * Special logic for `readObject` method for packing case
   * @return Packing stats
   */
  def specialReadObjectPart(dataInfo: DataInfo): Seq[Stat] = {
    val pack =
      q"""
          val packed = ${dataInfo.termName}
            .pack(..${dataInfo.classParamNames})
        """
    val fieldsAssignments = dataInfo.optimizedParams.unzip._1 match {
      case Seq(p1) => Seq(q"${Term.Name("_" + p1.value)} = packed")
      case ps =>
        ps.zipWithIndex.map {
          case (param, ind) =>
            q"""${Term.Name("_" + param.value)} =
                   packed.${Term.Name(s"_${ind + 1}")}"""
        }
    }
    pack +: fieldsAssignments
  }

  object UnpackGettersStats extends DataStats {

    override val statsTag: DataStats.StatsTag = DataStats.GettersStats

    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      dataInfo.classParamsWithTypes.zip(dataInfo.bitPositions).map {
        case ((param, tpe), bitPos) =>
          val fieldName = Term.Name("_" + param.value)

          val typeWithoutOption = tpe match {
            case t"Option[$t]" if dataInfo.dataMods.optimiseHeapOptions => t
            case t                                                      => t
          }

          val fieldGetter = bitPos.booleanBit match {
            case Some(bBit) => q"(_bitmask & (1 << ${Lit.Int(bBit)})) != 0"
            case None =>
              typeWithoutOption match {
                case t"String" if dataInfo.dataMods.optimiseHeapStrings =>
                  q"new String(this.$fieldName)"
                case _ =>
                  q"this.$fieldName"
              }
          }

          @SuppressWarnings(Array("org.wartremover.warts.Null"))
          val fullGetter = bitPos.optionBit match {
            case Some(oBit) =>
              q"""
                 if ((_bitmask & (1 << ${Lit.Int(oBit)})) != 0) {
                   None
                 } else {
                   Some($fieldGetter)
                 }
               """
            case None =>
              tpe match {
                case t"Option[$_]" if dataInfo.dataMods.optimiseHeapOptions =>
                  q"""
                   if (this.$fieldName == null) {
                     None
                   } else {
                     Some($fieldGetter)
                   }
                 """
                case _ => fieldGetter
              }
          }
          q"def $param: $tpe = $fullGetter"
      }
  }

  object PackStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val params = dataInfo.classParamsWithTypes.map {
        case (param, tpe) => param"$param: $tpe"
      }
      val packedType = dataInfo.optimizedParams.unzip._2 match {
        case Seq(tpe1) => tpe1
        case tpes      => t"(..$tpes)"
      }

      val packs = dataInfo.classParamsWithTypes.zip(dataInfo.bitPositions).map {
        case ((param, tpe), bitPos) =>
          val fieldName = Term.Name("_" + param.value)

          val (typeWithoutOption, paramAccess: Term) = tpe match {
            case t"Option[$t]" if dataInfo.dataMods.optimiseHeapOptions =>
              (t, q"$param.get")
            case t =>
              (t, q"$param")
          }

          val (fieldPack: Term, needVal) = bitPos.booleanBit match {
            case Some(bBit) =>
              (q"""if ($paramAccess) {
                     _bitmask |= (1 << ${Lit.Int(bBit)})
                   }""", false)
            case None =>
              typeWithoutOption match {
                case t"String" if dataInfo.dataMods.optimiseHeapStrings =>
                  (q"$paramAccess.getBytes", true)
                case _ =>
                  (q"$paramAccess", true)
              }
          }

          @SuppressWarnings(Array("org.wartremover.warts.Null"))
          val fullPack: Term = bitPos.optionBit match {
            case Some(oBit) =>
              val dummyVal = if (needVal) {
                Seq(DataInfo.dummyValForPrimitive(typeWithoutOption))
              } else {
                Seq()
              }
              q"""
                  if ($param == None) {
                    _bitmask |= (1 << ${Lit.Int(oBit)})
                    ..$dummyVal
                  } else {
                    $fieldPack
                  }
               """
            case None =>
              tpe match {
                case t"Option[$_]" if dataInfo.dataMods.optimiseHeapOptions =>
                  q"""
                   if ($param == None) {
                     null
                   } else {
                     $fieldPack
                   }
                 """
                case _ => fieldPack
              }
          }

          if (needVal) {
            q"val ${Pat.Var.Term(fieldName)} = $fullPack"
          } else {
            fullPack
          }
      }

      val optionalBitmask = if (dataInfo.hasBitmask) {
        Seq(q"var _bitmask: Long = 0L")
      } else {
        Seq()
      }

      val output = dataInfo.optimizedParams.unzip._1
        .map(param => Term.Name("_" + param.value)) match {
        case Seq(p1) => p1
        case ps      => q"(..$ps)"
      }

      Seq(
        q"""private def pack(..$params): $packedType = {
          ..$optionalBitmask
          ..$packs
          $output
         }
       """
      )
    }
  }
}
