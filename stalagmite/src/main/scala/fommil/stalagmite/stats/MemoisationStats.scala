// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package fommil.stalagmite.stats

import fommil.stalagmite.{ DataInfo, DataStats, MetaUtils }

import scala.collection.immutable.Seq
import scala.meta._

object MemoisationStats {

  object MemoisationStatsHelper {
    def produceMemoisedParams(
      dataInfo: DataInfo,
      paramProduce: (Term.Name, Type) => Term
    ): Seq[Defn.Val] =
      dataInfo.classParamsWithTypes.filter {
        case (param, _) =>
          dataInfo.dataMods.memoiseRefs.contains(param.value)
      }.map {
        case (param, tpe) =>
          q"""val ${Pat.Var.Term(Term.Name(param.value + "_memoised"))} =
                   ${paramProduce(param, tpe)}"""
      }

    def argsWithMemoised(dataInfo: DataInfo): Seq[Term.Name] =
      dataInfo.classParamNames.map(
        param =>
          if (dataInfo.dataMods.memoiseRefs.contains(param.value)) {
            Term.Name(param.value + "_memoised")
          } else {
            param
        }
      )

    def keyType(dataInfo: DataInfo): Type = {
      val typesWithoutTypeParams = dataInfo.classParamsTypes.map(
        tpe =>
          MetaUtils.replaceType(
            tpe,
            dataInfo.typeParamsNames.map(name => name.value -> t"AnyRef").toMap
        )
      )
      typesWithoutTypeParams match {
        case Seq(tpe1) => tpe1
        case tpes      => t"(..$tpes)"
      }
    }
  }

  object DataStrongMemoiseApplyStats extends DataStats {

    import MemoisationStatsHelper._
    import CaseClassStats.DataApplyStats

    override val statsTag: DataStats.StatsTag = DataStats.ApplyStats

    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val memoisedParams = produceMemoisedParams(
        dataInfo,
        (param, tpe) => q"memoisedRef_cache.intern($param).asInstanceOf[$tpe]"
      )

      def interning(varName: Term.Name) = {
        val nameWithValueEquality =
          Ctor.Ref.Name(dataInfo.name.value + "WithValueEquality")
        val wrapperCreating = if (dataInfo.typeParams.nonEmpty) {
          q"new $nameWithValueEquality[..${dataInfo.typeParamsNames}]($varName)"
        } else {
          q"new $nameWithValueEquality($varName)"
        }
        q"memoised_cache.intern($wrapperCreating).d"
      }

      val publishing = if (dataInfo.requiresToHaveVars) {
        Seq(q"val safe = created.synchronized(created)", interning(q"safe"))
      } else {
        Seq(interning(q"created"))
      }

      Seq(q"""
        def apply[..${dataInfo.simpleTypeParams}](
          ..${DataApplyStats.applyParams(dataInfo)}
          ): ${dataInfo.dataType} = {
            ..$memoisedParams
            ..${Seq(q"""
              val created = new ${Ctor.Ref.Name(dataInfo.name.value)}(
              ..${argsWithMemoised(dataInfo)})""")}
            ..$publishing
            }""")
    }
  }

  object DataStrongMemoiseStats extends DataStats {
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
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
            paramName => q"this.d.$paramName == that.d.$paramName"
          )
          eqs match {
            case Seq(eq1) =>
              eq1
            case Seq(eq1, rest @ _*) =>
              rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
          }
        }
        val wrapperCase = if (dataInfo.dataMods.memoiseHashCode) {
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

      val memoisedRefsCache =
        q"""
           private[this] val memoisedRef_cache =
            _root_.com.google.common.collect.Interners.newStrongInterner[AnyRef]()
         """

      Seq(
        dataWrapper,
        q"""private[this] val memoised_cache =
              _root_.com.google.common.collect.Interners.newStrongInterner
                [$wrapperWildcardType]()"""
      ) ++ (if (dataInfo.dataMods.memoiseRefs.nonEmpty) {
              Seq(memoisedRefsCache)
            } else {
              Seq()
            })
    }
  }

  object DataWeakMemoiseApplyStats extends DataStats {

    import MemoisationStatsHelper._
    import CaseClassStats.DataApplyStats

    override val statsTag: DataStats.StatsTag = DataStats.ApplyStats

    @SuppressWarnings(Array("org.wartremover.warts.Null"))
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val memoisedParams = produceMemoisedParams(
        dataInfo,
        (param, tpe) => q"memoisedRef($param).asInstanceOf[$tpe]"
      )

      val argsWithMemoised = MemoisationStatsHelper.argsWithMemoised(dataInfo)

      val keyVal = argsWithMemoised match {
        case Seq(arg1) => arg1
        case args      => q"(..$args)"
      }

      val publishing = if (dataInfo.requiresToHaveVars) {
        Seq(q"val safe = created.synchronized(created)", q"safe")
      } else {
        Seq(q"created")
      }

      Seq(q"""
        def apply[..${dataInfo.simpleTypeParams}](
          ..${DataApplyStats.applyParams(dataInfo)}
          ): ${dataInfo.dataType} = {
          ..$memoisedParams
          ..${Seq( // really ugly, don't know how to fix though
        q"val key: ${MemoisationStatsHelper.keyType(dataInfo)} = $keyVal",
        q"""val first = {
                  val weak = memoised_cache.get(key)
                  if (weak == null) null else weak.get
                }"""
      )}

          if (first != null) {
            first
          } else {
            memoised_cache.synchronized {
              val got = {
                val weak = memoised_cache.get(key)
                if (weak == null) {
                  null
                } else {
                  val ref = weak.get
                  ref
                }
              }
              if (got != null) {
                got
              } else {
                val created = new ${Ctor.Ref.Name(dataInfo.name.value)}(
                    ..${argsWithMemoised :+ Term.Name("key")}
                  )
                memoised_cache.put(key, new _root_.java.lang.ref
                  .WeakReference(created))
                ..$publishing
              }
            }
          }
        }""")
    }
  }

  object DataWeakMemoiseStats extends DataStats {
    @SuppressWarnings(Array("org.wartremover.warts.Null"))
    override def objectStats(dataInfo: DataInfo): Seq[Stat] = {
      val dataWildCardType = if (dataInfo.typeParams.nonEmpty) {
        t"""${Type.Name(dataInfo.name.value)}
             [..${Seq.fill(dataInfo.typeParams.length)(t"_")}]"""
      } else {
        Type.Name(dataInfo.name.value)
      }

      val memoisedCache =
        q"""
             private[this] val memoised_cache =
              new _root_.java.util.WeakHashMap[
                 ${MemoisationStatsHelper.keyType(dataInfo)},
                 _root_.java.lang.ref.WeakReference[$dataWildCardType]
               ]
            """

      val memoisedRefsCache =
        q"""
           private[this] val memoisedRef_cache =
            new _root_.java.util.WeakHashMap[
              AnyRef,
              _root_.java.lang.ref.WeakReference[AnyRef]]
         """
      val memoisedRefFun =
        q"""
           private[this] def memoisedRef(ref: AnyRef): AnyRef = {
               val first = {
                 val weak = memoisedRef_cache.get(ref)
                 if (weak == null) null else weak.get
               }
               if (first != null) first
               else
                 memoisedRef_cache.synchronized {
                   val got = {
                     val weak = memoisedRef_cache.get(ref)
                     if (weak == null) null else weak.get
                   }
                   if (got != null) got
                   else {
                     memoisedRef_cache.put(ref, new _root_.java.lang.ref
                      .WeakReference(ref))
                     ref
                   }
                 }
             }
         """

      Seq(memoisedCache) ++ (if (dataInfo.dataMods.memoiseRefs.nonEmpty) {
                               Seq(memoisedRefsCache, memoisedRefFun)
                             } else {
                               Seq()
                             })
    }
  }

}
