// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.apache.org/licenses/LICENSE-2.0
package fommil.stalagmite

import scala.collection.immutable.Seq
import scala.meta._

trait DataStats {
  def classStats(dataInfo: DataInfo): Seq[Stat]  = Seq()
  def objectStats(dataInfo: DataInfo): Seq[Stat] = Seq()
}
