package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class EqualsBenchmark {
  import BenchmarkData._

  // jmh:run -i 15 -wi 15 -f1 -t10 .*EqualsBenchmark
  // Benchmark                               Mode  Cnt   Score   Error  Units
  // EqualsBenchmark.caseClass              thrpt   15  37.869 ± 1.543  ops/s
  // EqualsBenchmark.caseClassMeta          thrpt   15  36.367 ± 2.427  ops/s
  // EqualsBenchmark.caseClassSpec          thrpt   15  36.418 ± 1.545  ops/s
  // EqualsBenchmark.memoisedCaseClass      thrpt   15  38.529 ± 2.812  ops/s
  // EqualsBenchmark.memoisedMeta           thrpt   15  45.929 ± 2.676  ops/s
  // EqualsBenchmark.memoisedSpec           thrpt   15  53.007 ± 2.291  ops/s
  // EqualsBenchmark.memoisedWeak           thrpt   15  44.041 ± 1.910  ops/s
  // EqualsBenchmark.optimizeHeapCaseClass  thrpt   15  29.972 ± 2.520  ops/s
  // EqualsBenchmark.optimizeHeapMeta       thrpt   15  11.915 ± 0.626  ops/s
  // EqualsBenchmark.optimizeHeapSpec       thrpt   15  11.020 ± 0.653  ops/s

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) => cs.foos(ind1) == cs.foos(ind2)
    }

  @Benchmark
  def caseClassSpec(cs: CaseClassData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) => cs.foosSpec(ind1) == cs.foosSpec(ind2)
    }

  @Benchmark
  def caseClassMeta(cs: CaseClassData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) => cs.foosMeta(ind1) == cs.foosMeta(ind2)
    }

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(oh: OptimizeHeapData,
                            e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        oh.foos(ind1) == oh.foos(ind2)
    }

  @Benchmark
  def optimizeHeapSpec(oh: OptimizeHeapData,
                       e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        oh.foosSpec(ind1) == oh.foosSpec(ind2)
    }

  @Benchmark
  def optimizeHeapMeta(oh: OptimizeHeapData,
                       e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        oh.foosMeta(ind1) == oh.foosMeta(ind2)
    }

  // memoised

  @Benchmark
  def memoisedCaseClass(m: MemoisedData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        m.foos(ind1) == m.foos(ind2)
    }

  @Benchmark
  def memoisedSpec(m: MemoisedData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        m.foosSpec(ind1) == m.foosSpec(ind2)
    }

  @Benchmark
  def memoisedMeta(m: MemoisedData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        m.foosMeta(ind1) == m.foosMeta(ind2)
    }

  @Benchmark
  def memoisedWeak(m: MemoisedData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        m.foosWeak(ind1) == m.foosWeak(ind2)
    }
}
