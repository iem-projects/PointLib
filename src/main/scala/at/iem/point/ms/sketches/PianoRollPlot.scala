/*
 *  PianoRollPlot.scala
 *  (PointLib - ms)
 *
 *  Copyright (c) 2013-2014 IEM Graz / Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package at.iem.point.ms.sketches

import java.awt.Color

import at.iem.point.illism.gui.PianoRoll
import at.iem.point.illism._
import at.iem.point.illism.gui.PianoRoll.NoteDecoration

import scala.collection.breakOut
import scala.swing.{Component, MainFrame, Frame, SimpleSwingApplication}

import de.sciss.pdflitz

object PianoRollPlot extends SimpleSwingApplication {
  lazy val top: Frame = {
    val comp = PianoRoll.j()
    // comp.showLines    = false
    // comp.showKeyboard = false
    val study = allPromising(1)
    println(s"study: $study")
    val seq = load(study)
    val notes: Vec[Vec[OffsetNote]] = Vector.tabulate(4)(ch => seq.notes(channel = ch))
    val notesFlat = notes.flatten
    comp.notes  = notesFlat
    val allPch  = notes.flatten.map(_.pitch.midi)
    val minPch  = allPch.min / 12 * 12
    val maxPch  = allPch.max + 1 // (allPch.max + 11) / 12 * 12
    // println(s"min pitch $minPch, max pitch $maxPch")
    comp.pitchRange = (minPch, maxPch)
    comp.timeRange = comp.timeRange._1 -> (comp.timeRange._2 /* * 0.5 */)

    val colors = Vector(
      Color.red, // Color.lightGray,
      Color.green, // Color.gray,
      Color.blue, // Color.darkGray,
      Color.yellow // Color.black
    )

    comp.decoration = notes.zipWithIndex.flatMap { case (ns, ch) =>
      val deco = NoteDecoration(Some(colors(ch)))
      ns.map(_ -> deco)
    } (breakOut)
    // comp.keyWidth = ...
    // comp.keyHeight = ...

    val c = Component.wrap(comp)

    new MainFrame {
      contents = c
      new pdflitz.SaveAction(c :: Nil).setupMenu(this)
    }
  }
}
