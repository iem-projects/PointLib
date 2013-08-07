package at.iem.point.ot.sketches

import de.sciss.muta

object GeneticApp extends muta.gui.GeneticApp(GeneticSystem) {
  override def rowHeight = 64
}