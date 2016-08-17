

package core

import scala.swing.event.ButtonClicked
import scala.swing.event.SelectionChanged
import scala.swing.Swing
import palico.SupportMoves
import scala.swing.MainFrame
import scala.swing.event.ListChanged
import scala.swing.FlowPanel
import scala.swing.BorderPanel
import scala.swing.Orientation
import palico.Skills
import scala.swing.BoxPanel
import scala.swing.event.WindowActivated
import palico.PBListItem
import scala.swing.ProgressBar
import scala.collection.mutable.ListBuffer
import javax.swing.ImageIcon
import scala.swing.GridPanel
import scala.swing.ListView
import scala.swing.Dialog
import scala.swing.MenuItem
import scala.swing.Menu
import scala.swing.Button
import scala.swing.Label
import scala.swing.ScrollPane
import scala.swing.MenuBar
import scala.swing.Action
import java.awt.Insets
import java.awt.Dimension
import java.awt.Color
import scala.reflect.runtime.universe.{TypeTag, typeOf}
import java.awt.Font
import java.util.Locale

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
//        new MenuItem(Action("Open") { println("open") }),
//        new MenuItem(Action("Save") { println("save") }),
//        new Separator,
        new MenuItem(Action("Quit") { PBmain.close }))
    },
    new Menu("Help") {
      contents += (
        new MenuItem(Action("About") {
          Dialog.showMessage(
              PBListsBox, 
              "Credits to Arkaether \n\nAll information gathered from \nhttps://redd.it/4tfy86", 
              "About", 
              Dialog.Message.Info, 
              Swing.EmptyIcon)
        }))
    })
}

object PBLayout extends BorderPanel {
  add(PBClassBox, BorderPanel.Position.North)
  add(PBMainContents, BorderPanel.Position.Center)
  add(PBStatusBar, BorderPanel.Position.South)
  
}

object PBStatusBar extends swing.GridPanel(1,4) {
  val bhernaVal = new Label(100.0 + "%")
  val kokotoVal = new Label(100.0 + "%")
  val pokkeVal = new Label(100.0 + "%")
  val yukumoVal = new Label(100.0 + "%")
  update
  
  var bhernaLbl = new FlowPanel { contents += (new Label("Bherna: "), bhernaVal) }
  var kokotoLbl = new FlowPanel { contents += (new Label("Kokoto: "), kokotoVal) }
  var pokkeLbl = new FlowPanel { contents += (new Label("Pokke: "), pokkeVal) }
  var yukumoLbl = new FlowPanel { contents += (new Label("Yukumo: "), yukumoVal) }
  
  contents += (bhernaLbl, kokotoLbl, pokkeLbl, yukumoLbl)
  listenTo(PBListsBox.moveView.listView, PBListsBox.skillView.listView)
  reactions += {
    case e: ListChanged[_] => update
  }
  def update() {
    var bherna, kokoto, pokke, yukumo = 1.0
    palico.Cat.moveListBuffer.foreach { x =>
      bherna *= x.bhernaRate
      kokoto *= x.kokotoRate
      pokke *= x.pokkeRate
      yukumo *= x.yukumoRate
    }
    palico.Cat.skillListBuffer.foreach { x =>
      bherna *= x.bhernaRate
      kokoto *= x.kokotoRate
      pokke *= x.pokkeRate
      yukumo *= x.yukumoRate
    }
    bhernaVal.text = "%.9f".formatLocal(Locale.US, bherna * 100).toFloat.toString
    kokotoVal.text = "%.9f".formatLocal(Locale.US, kokoto * 100).toFloat.toString
    pokkeVal.text = "%.9f".formatLocal(Locale.US, pokke * 100).toFloat.toString
    yukumoVal.text = "%.9f".formatLocal(Locale.US, yukumo * 100).toFloat.toString
    val max = List(bherna,kokoto,pokke,yukumo).map(d => "%.9f".formatLocal(Locale.US, d * 100).toFloat).max
    List(bhernaVal,kokotoVal,pokkeVal,yukumoVal).foreach { lbl =>
      if(max != 100.0 && lbl.text.toFloat == max) {
        lbl.font = new Font(lbl.font.getFamily, Font.BOLD, lbl.font.getSize)
        lbl.border = Swing.MatteBorder(0, 0, 1, 0, Color.RED)
      }
      else {
        lbl.font = new Font(lbl.font.getFamily, Font.PLAIN, lbl.font.getSize)
        lbl.border = Swing.EmptyBorder(0, 0, 1, 0)
      }
      lbl.text += "%"
    }
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
  var learnLabel = new Label("learn something")
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
      if(palico.Cat.getInnate[T].exists(i => i.isInstanceOf[T] && i.toString == learnLabel.text))
        learnLabel.text = "learn something"
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

