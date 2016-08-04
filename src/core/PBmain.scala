

package core

import scala.swing.MainFrame
import scala.swing.MenuBar
import scala.swing.Menu
import scala.swing.MenuItem
import scala.swing.Separator
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.event.SelectionChanged
import scala.swing.ComboBox
import scala.swing.Swing
import java.awt.Color
import palico.Rath_of_Meow
import scala.swing.BoxPanel
import java.awt.Dimension
import scala.swing.event.ButtonClicked
import java.awt.Point
import palico.SupportMoves
import palico.Skills
import scala.swing.ScrollPane
import scala.swing.event.WindowDeactivated
import scala.swing.Action
import scala.swing.Orientation
import scala.swing.FlowPanel
import javax.swing.ImageIcon
import scala.swing.BorderPanel
import scala.swing.Alignment
import scala.swing.ListView
import scala.swing.Button
import scala.swing.ProgressBar
import java.awt.Insets
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag
import palico.PBListItem
import palico.PBListItem
import scala.swing.event.ListChanged
import java.awt.Font

object PBmain extends MainFrame with App {
  title = Ref.MAIN_TITLE
  size = Ref.MAIN_SIZE
  minimumSize = size
  maximumSize = size
  resizable = false
  menuBar = PBMenuBar
  contents = PBLayout
  centerOnScreen
  open
}

object PBMenuBar extends MenuBar {
  contents += (
    new Menu("File") {
      contents += (
        new MenuItem(Action("Open") { println("open") }),
        new MenuItem(Action("Save") { println("save") }),
        new Separator,
        new MenuItem(Action("Quit") { System.exit(0) }))
    })
}

object PBLayout extends BorderPanel {
  add(PBClassBox, BorderPanel.Position.North)
  add(PBMainContents, BorderPanel.Position.Center)
  add(PBStatusBar, BorderPanel.Position.South)
  
}

object PBStatusBar extends swing.GridPanel(1,4) {
  def bhernaVal = new Label(100 + "%")
  def kokotoVal = new Label(100 + "%")
  def pokkeVal = new Label(100 + "%")
  def yukumoVal = new Label(100 + "%")
  
  var bhernaLbl = new FlowPanel { contents += (new Label("Bherna: "), bhernaVal) }
  var kokotoLbl = new FlowPanel { contents += (new Label("Kokoto: "), kokotoVal) }
  var pokkeLbl = new FlowPanel { contents += (new Label("Pokke: "), pokkeVal) }
  var yukumoLbl = new FlowPanel { contents += (new Label("Yukumo: "), yukumoVal) }
  
  contents += (bhernaLbl, kokotoLbl, pokkeLbl, yukumoLbl)
  
  def update() {
    bhernaVal.text = 90 + "%"
    kokotoVal.text = 90 + "%"
    pokkeVal.text = 90 + "%"
    yukumoVal.text = 90 + "%"
  }
}

object PBClassBox extends BoxPanel(Orientation.Horizontal) {
  var icon =(new ImageIcon(""))
  val classCb = new swing.ComboBox(List(palico.Charisma, palico.Fighting, palico.Protection, palico.Assisting, palico.Healing, palico.Bombing, palico.Gathering)) {
    makeBorder
    listenTo(selection)
    reactions += {
      case e: SelectionChanged if (selection.item != palico.Cat.catClass) => //println(selection.item)
        palico.Cat.catClass = selection.item
        palico.Cat.publish(SelectionChanged(this))
        makeBorder
        // change icon
    }
    def makeBorder() {
      border = null
      border = Swing.TitledBorder(Swing.MatteBorder(3, 0, 0, 0, palico.Cat.catClass.classColor), "Palico Class")
    }
  }
  
  val iconPanel = new FlowPanel { }
  
  border = Swing.EmptyBorder(5)
  contents += (classCb, iconPanel)
}

class PBLabel(fn: => String) extends Label(fn) {
  listenTo(palico.Cat)
  reactions += {
    case SelectionChanged(_) => text = fn
      //println("changing " +text+ " to " + fn)
  }
}

object PBMoveBox extends GridPanel(1,2) {
//  border = Swing.LineBorder(Color.BLACK)
  var defaultMoveLbl = new PBLabel(palico.Cat.defaultMove.toString)
  var secondaryMovesLbl = new PBLabel(palico.Cat.secondaryMoves.head +"\n -or- \n"+ palico.Cat.secondaryMoves.last)
  var moveBox = new GridPanel(2,1) {
    contents += (defaultMoveLbl, secondaryMovesLbl)
    border = Swing.TitledBorder(Swing.MatteBorder(1, 1, 1, 1, Color.BLACK), "Default Moves")
  }
  contents += (moveBox) 
  
  var defaultSkillsLbl = new PBLabel(palico.Cat.defaultSkills.head + "\n -and- \n" + palico.Cat.defaultSkills.last)
  var skillBox = new GridPanel(1,1) {
    contents += defaultSkillsLbl
    border = Swing.TitledBorder(Swing.MatteBorder(1, 1, 1, 1, Color.BLACK), "Default Skills")
  }
  contents += (skillBox)
}

class PBListViewButton(s: String) extends Button(s) {
  border = Swing.CompoundBorder(Swing.EmptyBorder(1), border)
  maximumSize = new Dimension(30, size.height)
  minimumSize = maximumSize
  margin = new Insets(0,0,0,0)
  focusable = false
}

class PBProgressBar[T](m: => Int, c: => Int)(implicit parent: ListView[T]) extends ProgressBar {
  val defaultForeground = foreground
  orientation = Orientation.Vertical
  minimumSize = new Dimension(30, size.height)
  max = m
  value = max - c
  border = Swing.CompoundBorder(Swing.EmptyBorder(1), border)
  label = setLabel
  labelPainted = true
  listenTo(parent)
  reactions += {
    case e: ListChanged[T] => 
      value = max - c
      label = setLabel
  }
  
  def setLabel(): String = {
    if(value >= max) {
      font = new Font(font.getFontName,Font.BOLD,font.getSize)
      foreground = Color.RED
      "MAX"
    } else {
      font = new Font(font.getFontName,Font.PLAIN,font.getSize)
      foreground = defaultForeground
      value.toString
    }
  }
}

class PBListView[T <: PBListItem](l: => ListBuffer[T])(implicit tag: TypeTag[T]) extends BorderPanel {
  border = Swing.EmptyBorder(3)
  implicit var listView = new ListView[T](l)
  var listPane = new ScrollPane(listView)
  add(listPane, BorderPanel.Position.Center)
  
  var plusButton = new PBListViewButton("+") { tooltip = "Add to list" }
  var minusButton = new PBListViewButton("-") { tooltip = "Remove selected" }
  var buttons = new GridPanel(2,1) { border = Swing.EmptyBorder(1) }
  buttons.contents += (plusButton, minusButton)
  var progressBar = new PBProgressBar(palico.Cat.getMaxCost[T], palico.Cat.availablePoints[T] ) { }
  var rightPanel = new BoxPanel(Orientation.Vertical)
  rightPanel.contents += (buttons, progressBar)
  rightPanel.border = Swing.MatteBorder(1, 0, 1, 1, Color.GRAY)
  add(rightPanel, BorderPanel.Position.East)
  
  var learnedBox = new BoxPanel(Orientation.Horizontal) { border = Swing.LineBorder(Color.BLACK) }
  learnedBox.contents += new Label("learned thing here")
  add(learnedBox, BorderPanel.Position.South)
  
  listenTo(plusButton,minusButton)
  reactions += {
    case e: ButtonClicked if(e.source == minusButton) =>
      if(listView.selection.items.nonEmpty) listView.selection.items.foreach { l -= _ }
      listView.listData = l
      listView.publish(ListChanged[T](listView))
  }
}

object PBListsBox extends GridPanel(1,2) {
  var moveView = new PBListView[SupportMoves](palico.Cat.moveListBuffer)
  var skillView = new PBListView[Skills](palico.Cat.skillListBuffer)
  contents += (moveView, skillView)
}

object PBMainContents extends BoxPanel(Orientation.Vertical) {
  
  val supportButtons = new swing.BoxPanel(swing.Orientation.Vertical) {
    val buttonPlus = new swing.Button("+") {
      name = "support"
      margin = new java.awt.Insets(0, 0, 0, 0); minimumSize = new Dimension(20, 20); maximumSize = minimumSize; focusable = false
      listenTo(this)
      reactions += {
        case e: ButtonClicked => //println("new window")
         listenTo(new Picker(name))
      }
    }
    
    val buttonMinus = new swing.Button("-") { margin = new java.awt.Insets(0,0,0,0); minimumSize = new Dimension(20,20); maximumSize = minimumSize; focusable = false }
    contents += (buttonPlus, buttonMinus)
  }
  val supportMoveListView = new swing.ListView[palico.SupportMoves]() { 
    listData = palico.Cat.moveListBuffer
    border = Swing.CompoundBorder(Swing.BeveledBorder(Swing.Raised), Swing.BeveledBorder(Swing.Lowered))
    listenTo(supportButtons.buttonMinus)
    reactions += {
      case e: ButtonClicked if(selection.items.nonEmpty) => //println(e)
        selection.items.foreach { palico.Cat.moveListBuffer -= _ }
        listData = palico.Cat.moveListBuffer
    }
  }
  val supportMoveContainer = new swing.BorderPanel() {
    add(supportMoveListView, swing.BorderPanel.Position.Center)
    add(supportButtons, swing.BorderPanel.Position.East)
    //border = Swing.LineBorder(Color.GRAY)
  }
  
  
  val skillsButtons = new swing.BoxPanel(swing.Orientation.Vertical) {
    val buttonPlus = new swing.Button("+") { margin = new java.awt.Insets(0,0,0,0); minimumSize = new Dimension(20,20); maximumSize = minimumSize; focusable = false }
    val buttonMinus = new swing.Button("-") { margin = new java.awt.Insets(0,0,0,0); minimumSize = new Dimension(20,20); maximumSize = minimumSize; focusable = false }
    contents += (buttonPlus, buttonMinus)
  }
  val skillsListView = new swing.ListView[palico.Skills]() { 
    listData = palico.Cat.skillListBuffer 
    border = Swing.CompoundBorder(Swing.BeveledBorder(Swing.Raised), Swing.BeveledBorder(Swing.Lowered))
  }
  val skillsContainer = new swing.BorderPanel() {
    add(new ScrollPane(skillsListView), swing.BorderPanel.Position.Center)
    add(new ScrollPane(skillsButtons), swing.BorderPanel.Position.East)
    //border = Swing.LineBorder(Color.GRAY)
  }
  
  contents += (
      PBMoveBox,
      PBListsBox)
   border = Swing.EmptyBorder(5)
   
   
}

class Picker(val t: String) extends swing.Dialog {
  modal = true
  title = "Select " + t
  contents = new swing.BorderPanel {
    border = Swing.EmptyBorder(5)
    val listview = 
     // if(t == "support")
        new swing.ListView[SupportMoves](palico.SupportMoves.allMoves)
      //else new swing.ListView[Skills]()
    
    add(new swing.ScrollPane(listview), swing.BorderPanel.Position.Center)
    
    val buttons = new GridPanel(1,2) {
      contents += (new swing.Button("OK!") {name = "ok"}, new swing.Button("Cancel!") {name = "cancel"})
      hGap = 5
      vGap = 5
    }
    add(buttons, swing.BorderPanel.Position.South)
    listenTo(buttons.contents.head, buttons.contents.last)
    reactions += {
      case e: ButtonClicked if(e.source.name == "ok") => //println(e)
        palico.Cat.moveListBuffer += listview.selection.items.head
        close
      case e: ButtonClicked if(e.source.name == "cancel") => close
    }
  }
  
  
  open
}

