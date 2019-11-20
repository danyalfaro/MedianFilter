/**
 * Performs a Median Filter on an image (photo1) and outputs a new image (photo2)
 *
 * @author Daniel Alfaro R.
 *
 */

import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import java.util

case class Sequential(image: BufferedImage)
case class Parallel(image: BufferedImage)

class SequentialActor extends Actor {
  val startTime : Long = System.currentTimeMillis()
  def receive = {
    case Sequential(photo1) => {
      val w = photo1.getWidth
      val h = photo1.getHeight

      // create new image of the same size
      val photo2 = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      val windowWidth = 3
      val windowHeight = 3
      val edgex = windowWidth/2
      val edgey = windowHeight/2
      val window = Array.ofDim[Int](9)

      for (x <- edgex until (w - edgex)) {
        for (y <- edgey until (h - edgey)) {
          var i = 0
          for (fx <- 0 until windowWidth) {
            for (fy <- 0 until windowHeight) {
              window(i) = photo1.getRGB(x + fx - edgex, y + fy - edgey)
              i += 1
            }
          }
          val sortedWindow = window.sorted
          photo2.setRGB(x, y, sortedWindow(windowWidth*windowHeight/2))
        }
      }
      // save image to file "test.jpg"
      ImageIO.write(photo2, "jpg", new File("sequentialFilter.jpg"))
      val endTime : Long = System.currentTimeMillis()
      println("Sequential Time Ellapsed: " + (endTime - startTime) + "ms")
    }
  }
}

class ParallelActor extends Actor {
  val startTime2 : Long = System.currentTimeMillis()
  def receive = {
    case Parallel(photo1) => {
      val w = photo1.getWidth
      val h = photo1.getHeight

      // create new image of the same size
      val photo2 = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      val windowWidth = 3
      val windowHeight = 3
      val edgex = windowWidth/2
      val edgey = windowHeight/2
      val window = Array.ofDim[Int](9)

      for (x <- edgex until (w - edgex)) {
        for (y <- edgey until (h - edgey)) {
          var i = 0
            for (fx <- 0 until windowWidth) {
              for (fy <- 0 until windowHeight) {
                window(i) = photo1.getRGB(x + fx - edgex, y + fy - edgey)
                i += 1
              }
            }
          util.Arrays.parallelSort(window)
          photo2.setRGB(x, y, window(windowWidth*windowHeight/2))
        }
      }
      // save image to file "test.jpg"
      ImageIO.write(photo2, "jpg", new File("parallelFilter.jpg"))
      val endTime2 : Long = System.currentTimeMillis()
      println("Parallel Time Ellapsed: " + (endTime2 - startTime2) + "ms")
    }
  }
}

object medianFilter extends App {

  // an actor needs an ActorSystem
  val system = ActorSystem("HelloSystem")

  // create and start the actor
  val sequentialActor = system.actorOf(Props[SequentialActor], name = "sequentialActor")

  val photo1 = ImageIO.read(new File("photo.jpg"))

  sequentialActor ! Sequential(photo1)

  system.terminate()

  // an actor needs an ActorSystem
  val system2 = ActorSystem("HelloSystem")

  // create and start the actor
  val parallelActor = system2.actorOf(Props[ParallelActor], name = "parallelActor")

  //val startTime : Long = System.currentTimeMillis()
  parallelActor ! Parallel(photo1)

  system2.terminate()

}
