

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
import scala.reflect.runtime.universe.{TypeTag,typeOf}
import palico.PBListItem
import palico.PBListItem
import scala.swing.event.ListChanged
import java.awt.Font
import scala.swing.Dialog
import scala.swing.event.WindowActivated

object PBmain extends MainFrame with App {
  title = Ref.MAIN_TITLE
  size = Ref.MAIN_SIZE
  minimumSize = size
  maximumSize = size
  resizable = false
  menuBar = PBMenuBar
  contents = PBLayout
  centerOnScreen
  Picker
  listenTo(Picker)
  reactions += {
    case e: WindowActivated => visible = true
  }
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

class PBListViewButton[T](s: String)(implicit parent: TypeTag[T]) extends Button(s) {
  parent match {
    case _ if typeOf[T] == typeOf[SupportMoves] => name = "moves"
    case _ if typeOf[T] == typeOf[Skills] => name = "skills"
    case _ =>
  }
  border = Swing.CompoundBorder(Swing.EmptyBorder(1), border)
  //maximumSize = new Dimension(30, 30)
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
  var progressBar = new PBProgressBar(palico.Cat.getMaxCost[T], palico.Cat.availablePoints[T] )
  var rightPanel = new BoxPanel(Orientation.Vertical)
  rightPanel.contents += (buttons, progressBar)
  rightPanel.border = Swing.MatteBorder(1, 0, 1, 1, Color.GRAY)
  add(rightPanel, BorderPanel.Position.East)
  
  var learnedBox = new BoxPanel(Orientation.Horizontal) { border = Swing.LineBorder(Color.BLACK) }
  var learnButton = new PBListViewButton("x") { tooltip = "Learn something" }
  var learnLabel = new Label("learned thing here")
  learnedBox.contents += (learnButton, learnLabel)
  add(learnedBox, BorderPanel.Position.South)
  
  listenTo(minusButton, palico.Cat)
  reactions += {
    case e: ButtonClicked if(e.source == minusButton) =>
      if(listView.selection.items.nonEmpty) listView.selection.items.foreach { l -= _ }
      listView.listData = l.sortWith(_.cost > _.cost)
      listView.publish(ListChanged[T](listView))
    case e: ButtonClicked if(e.source == Picker.addButton && Picker.addButton.tooltip == "learn" ) =>
      palico.Cat.setLearned[T](Picker.listview.selection.items.head.asInstanceOf[T])
      learnLabel.text = palico.Cat.getLearned[T].toString
    case e: ButtonClicked if(e.source == Picker.addButton) =>
      l += Picker.listview.selection.items.head.asInstanceOf[T]
      listView.listData = l.sortWith(_.cost > _.cost)
      listView.publish(ListChanged[T](listView))
    case e: SelectionChanged if(e.source == PBClassBox.classCb)=>
      while(palico.Cat.availablePoints[T] < 0)
        l -= l.sortWith(_.cost > _.cost).last
      listView.listData = l.sortWith(_.cost > _.cost)
      listView.publish(ListChanged[T](listView))
  }
}

object PBListsBox extends GridPanel(1,2) {
  var moveView = new PBListView[SupportMoves](palico.Cat.moveListBuffer)
  var skillView = new PBListView[Skills](palico.Cat.skillListBuffer)
  contents += (moveView, skillView)
}

object PBMainContents extends BoxPanel(Orientation.Vertical) {
  contents += (
      PBMoveBox,
      PBListsBox)
   border = Swing.EmptyBorder(5)
}

object Picker extends Dialog {
  title = ""
  modal = true
  resizable = false
  setLocationRelativeTo(PBMainContents)
  var listview: ListView[_] = null
  val scrollPane = new ScrollPane
  val addButton = new Button(Action("Add"){close})
  contents = new BorderPanel {
    border = Swing.EmptyBorder(5)    
    add(scrollPane, BorderPanel.Position.Center)    
    val buttons = new GridPanel(1,2) {
      contents += (addButton, new Button(Action("Cancel"){close}) )
      hGap = 5
      vGap = 5
    }
    add(buttons, BorderPanel.Position.South)
  }
  
  def supportList = palico.Cat.getDescendants[SupportMoves].toList
  def skillsList = palico.Cat.getDescendants[Skills].toList
  listenTo(PBListsBox.moveView.plusButton, PBListsBox.skillView.plusButton,
      PBListsBox.moveView.learnButton, PBListsBox.skillView.learnButton)
  reactions += {
    case e: ButtonClicked if(e.source == PBListsBox.moveView.learnButton || e.source == PBListsBox.moveView.plusButton && palico.Cat.availablePoints[SupportMoves] > 0) => 
      addButton.tooltip = if(e.source == PBListsBox.moveView.learnButton) "learn" else "add"
      PBListsBox.moveView.listenTo(addButton)
      populateList(palico.Cat.getAvailable[SupportMoves](e.source == PBListsBox.moveView.learnButton))
      PBListsBox.moveView.deafTo(addButton)
    case e: ButtonClicked if(e.source == PBListsBox.skillView.learnButton || e.source == PBListsBox.skillView.plusButton && palico.Cat.availablePoints[Skills] > 0) =>
      addButton.tooltip = if(e.source == PBListsBox.skillView.learnButton) "learn" else "add"
      PBListsBox.skillView.listenTo(addButton)
      populateList(palico.Cat.getAvailable[Skills](e.source == PBListsBox.skillView.learnButton))
      PBListsBox.skillView.deafTo(addButton)
  }
  
  def populateList(l: => List[PBListItem]) {
    l.inits //instantiate objects, populate list
    val list = l.sortBy(_.toString).sortWith(_.cost > _.cost)
    val listcost = list.map(_.asInstanceOf[PBListItem].cost)
    scrollPane.rowHeaderView = new ListView(listcost.toList) {
      enabled = false
    }
    listview = new ListView(list) {
      selection.intervalMode = ListView.IntervalMode.Single
      selectIndices(0)
    }
    scrollPane.viewportView = listview
    pack
    setLocationRelativeTo(PBMainContents)
    open
  }
}

