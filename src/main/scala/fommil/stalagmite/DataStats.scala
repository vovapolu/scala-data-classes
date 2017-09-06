// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package fommil.stalagmite

import scala.collection.immutable.Seq
import scala.meta._

object DataStats {
  sealed trait StatsTag
  case object NoOverride   extends StatsTag
  case object ApplyStats   extends StatsTag
  case object GettersStats extends StatsTag
}

trait DataStats {
  import DataStats._

  def classStats(dataInfo: DataInfo): Seq[Stat]  = Seq()
  def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq()
  val statsTag: StatsTag                         = NoOverride
}
