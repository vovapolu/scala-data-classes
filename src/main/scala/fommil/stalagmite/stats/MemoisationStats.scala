package fommil.stalagmite.stats

import fommil.stalagmite.DataInfo
import fommil.stalagmite.DataStats

import scala.collection.immutable.Seq
import scala.meta._

object MemoisationStats {

  object DataMemoiseStats extends DataStats {
    override def classStats(dataInfo: DataInfo): Seq[Stat] =
      if (dataInfo.getMod("memoiseStrong")) {
        Seq()
      } else {
        Seq(
          q"def intern: ${dataInfo.dataType} = ${dataInfo.dataCreating}"
        )
      }

    override def objectStats(dataInfo: DataInfo): Seq[Stat] =
      if (dataInfo.getMod("memoiseStrong")) {
        val wrapperName = Type.Name(dataInfo.name.value + "WithValueEquality")
        val wrapperWildcardType = if (dataInfo.typeParams.nonEmpty) {
          t"$wrapperName[..${Seq.fill(dataInfo.typeParams.length)(t"_")}]"
        } else {
          wrapperName
        }

        val dataWrapper = {
          val wrapperPatType = if (dataInfo.typeParams.nonEmpty) {
            pt"$wrapperName[..${Seq.fill(dataInfo.typeParams.length)(pt"_")}]"
          } else {
            pt"$wrapperName"
          }
          val wrapperCaseBody = {
            val eqs = dataInfo.classParamNames.map(
              paramName => q"that.d.$paramName == this.d.$paramName"
            )
            eqs match {
              case Seq(eq1) =>
                eq1
              case Seq(eq1, rest @ _*) =>
                rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
            }
          }
          val wrapperCase = if (dataInfo.getMod("memoiseHashCode")) {
            p"""case that: $wrapperPatType
               if this.hashCode == that.hashCode => $wrapperCaseBody"""
          } else {
            p"""case that: $wrapperPatType => $wrapperCaseBody"""
          }
          q"""private class $wrapperName[..${dataInfo.simpleTypeParams}](
                val d: ${dataInfo.dataType}
              ) {
               override def toString: String = d.toString
               override def hashCode: Int = d.hashCode
               override def equals(o: Any): Boolean = o match {
                 case $wrapperCase
                 case _ => false
               }
             }"""
        }

        Seq(
          dataWrapper,
          q"""private[this] val memoised_cache =
              _root_.com.google.common.collect.Interners.newStrongInterner
                [$wrapperWildcardType]()"""
        ) ++ (if (dataInfo.extraParams.memoiseRefs.nonEmpty) {
                Seq(
                  q"""
                      private[this] val memoisedRef_cache =
                      _root_.com.google.common.collect.Interners.newStrongInterner
                        [AnyRef]()
                    """
                )
              } else {
                Seq()
              })
      } else {
        val dataWildCardType = if (dataInfo.typeParams.nonEmpty) {
          t"""${Type.Name(dataInfo.name.value)}
             [..${Seq.fill(dataInfo.typeParams.length)(t"_")}]"""
        } else {
          Type.Name(dataInfo.name.value)
        }

        Seq(
          q"""
              private[this] val memoised_cache =
              _root_.com.google.common.collect.Interners.newWeakInterner
                [$dataWildCardType]()
            """
        ) ++ (if (dataInfo.extraParams.memoiseRefs.nonEmpty) {
                Seq(
                  q"""
                      private[this] val memoisedRef_cache =
                      _root_.com.google.common.collect.Interners.newWeakInterner
                        [AnyRef]()
                    """
                )
              } else {
                Seq()
              })
      }
  }
}
