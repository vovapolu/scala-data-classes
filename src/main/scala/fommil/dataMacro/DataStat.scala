package fommil.dataMacro

import fommil.dataMacro.DataInfo.DataInfo

import scala.collection.immutable.Seq
import scala.meta._

object DataStat {

  trait DataStatBuilder {
    def classStats(dataInfo: DataInfo): Seq[Stat] = Seq()
    def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq()
  }

  object DataApplyBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val args = dataInfo.classParams.map(param => Term.Name(param.name.value))
      val params = dataInfo.classParams.map(param => param"${Term.Name(param.name.value)}: ${param.decltpe.get}")
      if (dataInfo.dataMods.intern) {
        Seq(q"""def apply[..${dataInfo.simpleTypeParams}](..$params): ${dataInfo.dataType} = {
          val newVal = new ${Ctor.Ref.Name(dataInfo.name.value)}(..$args)
          ${Term.Name(dataInfo.name.value)}.intern(newVal)
        }""")
      } else {
        Seq(q"""def apply[..${dataInfo.simpleTypeParams}](..$params): ${dataInfo.dataType} =
          new ${Ctor.Ref.Name(dataInfo.name.value)}(..$args)""")
      }
    }
  }

  object DataUnapplyBuilder extends DataStatBuilder {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val clFields = dataInfo.classParams.map(param => q"that.${Term.Name(param.name.value)}")

      val tupleTypes = if (dataInfo.classParams.length > 1) {
        t"(..${dataInfo.classTypes})"
      } else {
        t"${dataInfo.classTypes.head}"
      }
      val tupleArgs = if (clFields.length > 1) {
        q"(..$clFields)"
      } else {
        clFields.head
      }
      Seq(q"""def unapply[..${dataInfo.simpleTypeParams}](that: ${dataInfo.dataType}): Option[$tupleTypes] =
         Some($tupleArgs)""")
    }
  }


}
