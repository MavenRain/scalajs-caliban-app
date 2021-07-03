package tutorial.webapp

import concurrent.ExecutionContext.Implicits.global
import org.scalajs.dom.{document, Event, MouseEvent, Node}
import org.scalajs.dom.html.Document
import org.scalajs.dom.raw.Element
import scala.util.chaining.scalaUtilChainingOps
import sttp.client3.quick.{backend, UriContext}
import zio.ZIO.{fromEither, fromFuture}
import zio.Runtime.default.unsafeRunAsync_

val revision = 32
@main
def run =
  document.addContentLoadListener((_: Event) => setupUI())
  
def setupUI(): Unit =
  val button = document.createNewButton(
    content = "Click me!".toTextContent,
    clickEvent = (_: MouseEvent) => addClickedMessage()
  )
  Client
    .ColorQueries
    .characters(Client.Character.view)
    .toRequest(uri"http://localhost:8088/api")
    .send(backend)
    .map(_.body)
    .pipe(future => fromFuture(_ => future))
    .flatMap(fromEither(_))
    .map(response => document.addChildren(
      Seq(button, "Hello World".toParagraph)
      ++ response.fold(Seq[Paragraph]())(_.map(_.toString.toParagraph))
    ))
    .pipe(unsafeRunAsync_(_))

def addClickedMessage(): Unit =
  document.addChildren(Seq(s"You clicked the button!  Revision $revision".toParagraph))

opaque type Button = Element
opaque type TextContent = String
opaque type Paragraph = Element
opaque type ClickListener = MouseEvent => Unit
opaque type ContentLoadListener = Event => Unit
extension (clickListener: MouseEvent => Unit)
  def toClickListener: ClickListener = clickListener
extension (contentLoadListener: Event => Unit)
  def toContentLoadListener: ContentLoadListener = contentLoadListener
extension (string: String)
  def toTextContent: TextContent = string
  def toParagraph: Paragraph =
    document
      .createElement("p")
      .tap(_.textContent = string)
extension (doc: Document)
  def createNewButton(content: TextContent, clickEvent: ClickListener): Button =
    doc
      .createElement("button")
      .tap(button => button.textContent = content)
      .tap(button =>
        button.addEventListener("click", clickEvent)
      )
  def addChildren(children: Seq[Element]) =
    children.foreach(doc.body.appendChild(_))
  def addContentLoadListener(listener: ContentLoadListener) = 
    document.addEventListener("DOMContentLoaded", listener)
extension (node: Node)
  def addChildren(children: Seq[Element]) =
    children.foreach(node.appendChild(_))
