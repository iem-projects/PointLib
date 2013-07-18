package at.iem.point.er.sketches

import java.awt.{Component, LinearGradientPaint, Graphics, Graphics2D, Color}
import javax.swing.Icon

class ProgressIcon(width: Int = 40, height: Int = 12) extends Icon {
  var min   =   0
  var max   = 100
  var value =   0

  private val pntGrad = new LinearGradientPaint(1f, 1f, width - 1, height - 1, Array(0f, 1f),
    Array(new Color(0x00, 0x80, 0xFF), new Color(0x60, 0xC0, 0xFF)))

  def paintIcon(c: Component, g: Graphics, x: Int, y: Int): Unit = {
    val bar = (math.max(min, math.min(max, value)) - min) * (width - 2) / (max - min)
    val g2  = g.asInstanceOf[Graphics2D]
    g2.translate(x, y)
    try {
      g2.setColor(Color.black)
      g2.drawRect(0, 0, width - 1, height - 1)
      g2.setPaint(pntGrad)
      g2.fillRect(1, 1, bar, height - 2)
    } finally {
      g2.translate(-x, -y)
    }
  }

  def getIconWidth  = width
  def getIconHeight = height
}