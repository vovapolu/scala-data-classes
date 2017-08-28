// Copyright: 2017 https://github.com/fommil/stalagmite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package fommil.stalagmite

import scala.meta._
import scala.collection.immutable.Seq

object MetaUtils {
  def replaceTypeName(tpe: Type.Name,
                      transform: Map[String, Type.Name]): Type.Name =
    transform.getOrElse(tpe.value, tpe)

  def replaceType(tpe: Type, transform: Map[String, Type.Name]): Type =
    tpe match {
      case t: Type.Name => replaceTypeName(t, transform)
      case Type.Apply(tApply, tsApplied) =>
        Type.Apply(
          replaceType(tApply, transform),
          tsApplied.map(replaceType(_, transform))
        )
      case Type.ApplyInfix(tApply1, tInfix, tApply2) =>
        Type.ApplyInfix(
          replaceType(tApply1, transform),
          replaceTypeName(tInfix, transform),
          replaceType(tApply2, transform)
        )
      case Type.With(tWith1, tWith2) =>
        Type.With(
          replaceType(tWith1, transform),
          replaceType(tWith2, transform)
        )
      case Type.Function(tArgs, tOut) =>
        Type.Function(
          tArgs.map(replaceTypeArg(_, transform)),
          replaceType(tOut, transform)
        )
      case Type.Tuple(ts) =>
        Type.Tuple(
          ts.map(replaceType(_, transform))
        )
      case _ => tpe // only basic cases for now
    }

  def replaceTypeArg(typeArg: Type.Arg,
                     transform: Map[String, Type.Name]): Type.Arg =
    typeArg match {
      case Type.Arg.ByName(tpe) =>
        Type.Arg.ByName(replaceType(tpe, transform))
      case Type.Arg.Repeated(tpe) =>
        Type.Arg.Repeated(replaceType(tpe, transform))
      case t: Type =>
        replaceType(t, transform)
      case _ => typeArg
    }

  def isPrimitiveType(tpe: Type): Boolean =
    tpe match {
      case t"Boolean" | t"Byte" | t"Short" | t"Char" | t"Int" | t"Long" |
          t"Float" | t"Double" =>
        true
      case _ => false
    }

  def dummyValForPrimitive(tpe: Type): Term =
    tpe match {
      case t"Boolean" => q"false"
      case t"Byte"    => q"0.toByte"
      case t"Short"   => q"0.toShort"
      case t"Char"    => q"0.toChar"
      case t"Int"     => q"0"
      case t"Long"    => q"0L"
      case t"Float"   => q"0.0f"
      case t"Double"  => q"0.0"
      case _          => q"_"
    }

  def statsWhen(condition: Boolean, stats: Seq[Stat]): Seq[Stat] =
    if (condition) {
      stats
    } else {
      Seq()
    }

  def statsWhen(condition: Boolean, stat: Stat): Seq[Stat] =
    statsWhen(condition, Seq(stat))

  def statsWhen(condition: Boolean, stats: Stat*): Seq[Stat] =
    statsWhen(condition, Seq(stats: _*))
}
