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

import at.iem.point.illism.gui.PianoRoll

import scala.swing.{MainFrame, Frame, SimpleSwingApplication}

object PianoRollPlot extends SimpleSwingApplication {
  lazy val top: Frame = {
    val comp = PianoRoll.j()
    comp.showLines    = false
    comp.showKeyboard = false

    new MainFrame {
      ???
    }
  }
}
