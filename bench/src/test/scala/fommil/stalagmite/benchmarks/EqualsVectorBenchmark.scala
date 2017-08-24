package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class EqualsVectorBenchmark {
  import BenchmarkData._

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
  def memoisedWeakSpec(m: MemoisedData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        m.foosWeakSpec(ind1) == m.foosWeakSpec(ind2)
    }

  @Benchmark
  def memoisedWeak(m: MemoisedData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        m.foosWeak(ind1) == m.foosWeak(ind2)
    }

  @Benchmark
  def memoisedIntern(m: MemoisedData, e: EqualsData): IndexedSeq[Boolean] =
    e.comparingPairs.map {
      case (ind1, ind2) =>
        m.foosInternMeta(ind1) == m.foosInternMeta(ind2)
    }
}
