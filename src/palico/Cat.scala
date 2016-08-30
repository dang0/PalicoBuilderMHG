package palico

import scala.collection.mutable.ListBuffer
import java.awt.Color
import scala.swing.Publisher
import scala.reflect.runtime.universe._
import core.PBListsBox
import core.PBTableModel


object Cat extends Publisher {
  var catClass: CatClass = Charisma
  def defaultMove(): SupportMoves = catClass.defaultMove
  def secondaryMoves(): List[SupportMoves] = catClass.secondaryMoves
  def defaultSkills(): List[Skills] = catClass.defaultSkills
  def fullMoveList(): List[SupportMoves] = defaultMoveList ++ moveListBuffer
  val defaultMoveList: List[SupportMoves] =  List(Mini_Barrel_Bombay, Herb_Horn)
  val moveListBuffer: ListBuffer[SupportMoves] = ListBuffer.empty
  val skillListBuffer: ListBuffer[Skills] = ListBuffer.empty
  def comboListBuffer: ListBuffer[PBListItem] = moveListBuffer ++ skillListBuffer
  var learnedMove: SupportMoves = null
  var learnedSkill: Skills = null
  
  final def MAX_SUPPORT_MOVES: Int = 12
  final def MAX_SUPPORT_COST: Int = 9
  final def MAX_SKILL_COST: Int = 8
  
  def getList[T <: PBListItem](implicit tag: TypeTag[T]): List[T] = tag match {
    case moves if typeOf[T] == typeOf[SupportMoves] => moveListBuffer.toList.asInstanceOf[List[T]]
    case skills if typeOf[T] == typeOf[Skills] => skillListBuffer.toList.asInstanceOf[List[T]]
    case _ => throw new Exception("")
  }
  
  def getMaxCost[T <: PBListItem](implicit tag: TypeTag[T]): Int = tag match {
    case moves if typeOf[T] == typeOf[SupportMoves] => MAX_SUPPORT_COST
    case skills if typeOf[T] == typeOf[Skills] => MAX_SKILL_COST
    case _ => -1
  }
  
  def setLearned[T <: PBListItem](learn: T)(implicit tag: TypeTag[T]) = tag match {
    case moves if typeOf[T] == typeOf[SupportMoves] => learnedMove = learn.asInstanceOf[SupportMoves]
    case skills if typeOf[T] == typeOf[Skills] => learnedSkill = learn.asInstanceOf[Skills]
    case _ => throw new Exception("")
  }
  
  def getLearned[T <: PBListItem](implicit tag: TypeTag[T]): T = tag match {
    case moves if typeOf[T] == typeOf[SupportMoves] => learnedMove.asInstanceOf[T]
    case skills if typeOf[T] == typeOf[Skills] => learnedSkill.asInstanceOf[T]
    case _ => throw new Exception("")
  }
  
  def getInnate[T <: PBListItem](implicit tag: TypeTag[T]): Set[T] = tag match {
    case moves if typeOf[T] == typeOf[SupportMoves] => secondaryMoves.map(_.asInstanceOf[T]).toSet
    case skills if typeOf[T] == typeOf[Skills] => defaultSkills.map(_.asInstanceOf[T]).toSet
    case _ => throw new Exception("")
  }
    
  def availablePoints[T <: PBListItem](implicit tag: TypeTag[T]): Int = {
    var total = 0
    if(typeOf[T] == typeOf[SupportMoves] && catClass != Charisma) total += 1
    getList[T].foreach { total += _.cost }
    getMaxCost[T] - total
  }
  
  def getDescendants[T: TypeTag]: Set[T] = {
    val rm = reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
    val internal = typeOf[T].typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol]
    if (internal.isSealed) {
      internal.sealedDescendants.filter(_.isCaseClass)
        .map(t => rm.reflectModule(rm.staticModule(t.asClass.fullName))
            .instance.asInstanceOf[T])
    }
    else Set.empty
  }
  
  def getAvailable[T <: PBListItem](learn: Boolean = false)(implicit tag: TypeTag[T]): List[T] = {
    var list = getDescendants[T]
    list.inits
    tag match {
      case noPoints if(!learn && availablePoints[T] < 1) => List.empty[T]
      case moves if(typeOf[T] == typeOf[SupportMoves]) =>
        if(moveListBuffer.filter(_.isInstanceOf[MoveGroupB]).size == 1
            && moveListBuffer.filter(_.isInstanceOf[MoveGroupC]).size == (if(catClass != Charisma) 4 else 5))
          list = getDescendants[MoveGroupB].map(_.asInstanceOf[T])
        var removals = getDescendants[DefaultMoves].map(_.asInstanceOf[T])
        removals = removals ++ moveListBuffer.map(_.asInstanceOf[T]).toSet
        if(!learn) 
          removals = removals ++ getDescendants[SecondaryMoves].map(_.asInstanceOf[T])
        if(moveListBuffer.exists(_.isInstanceOf[MoveGroupA]) || availablePoints[T] < 3) 
          removals = removals ++ getDescendants[MoveGroupA].map(_.asInstanceOf[T])
        if(availablePoints[T] < 2) 
          removals = removals ++ getDescendants[MoveGroupB].map(_.asInstanceOf[T])
        removals = removals ++ Set(getLearned[T]) ++ getInnate[T]
        removals.inits
        list = list -- removals
        list.toList
      case skills if(typeOf[T] == typeOf[Skills]) =>
        if(skillListBuffer.filter(_.isInstanceOf[SkillGroupB]).size == 1
            && skillListBuffer.filter(_.isInstanceOf[SkillGroupC]).size == 4)
          list = getDescendants[MoveGroupB].map(_.asInstanceOf[T])
        var removals = skillListBuffer.map(_.asInstanceOf[T]).toSet
        if(!learn) 
          removals = removals ++ getDescendants[DefaultSkills].map(_.asInstanceOf[T])
        if(skillListBuffer.exists(_.isInstanceOf[SkillGroupA]) || availablePoints[T] < 3) 
          removals = removals ++ getDescendants[SkillGroupA].map(_.asInstanceOf[T])
        if(availablePoints[T] < 2) 
          removals = removals ++ getDescendants[SkillGroupB].map(_.asInstanceOf[T])
        removals = removals ++ Set(getLearned[T]) ++ getInnate[T]
        removals.inits
        list = list -- removals
        list.toList
      case _ => List.empty[T]
    }
  }

  def declareMax {
    var rates = List(1.0, 1.0, 1.0, 1.0)
    var maxes: ListBuffer[PBListItem] = ListBuffer.empty[PBListItem]
    var moveMax, skillMax = 0.0
    moveListBuffer.foreach { x =>
      rates = (rates, List(x.bhernaRate, x.kokotoRate, x.pokkeRate, x.yukumoRate)).zipped.map(_ * _)
    }
    skillListBuffer.foreach { x =>
      rates = (rates, List(x.bhernaRate, x.kokotoRate, x.pokkeRate, x.yukumoRate)).zipped.map(_ * _)
    }
    moveListBuffer.foreach { x =>
      var removedRates = (rates, List(x.bhernaRate, x.kokotoRate, x.pokkeRate, x.yukumoRate)).zipped.map(_ / _)
      if (removedRates.max > moveMax) moveMax = removedRates.max
    }
    skillListBuffer.foreach { x =>
      var removedRates = (rates, List(x.bhernaRate, x.kokotoRate, x.pokkeRate, x.yukumoRate)).zipped.map(_ / _)
      if (removedRates.max > skillMax) skillMax = removedRates.max
    }
    moveListBuffer.foreach { x =>
      var removedRates = (rates, List(x.bhernaRate, x.kokotoRate, x.pokkeRate, x.yukumoRate)).zipped.map(_ / _)
      if (removedRates.max == moveMax) maxes += x
    }
    skillListBuffer.foreach { x =>
      var removedRates = (rates, List(x.bhernaRate, x.kokotoRate, x.pokkeRate, x.yukumoRate)).zipped.map(_ / _)
      if (removedRates.max == skillMax) maxes += x
    }
    comboListBuffer.intersect(maxes).foreach { _.isMax = true }
    comboListBuffer.diff(maxes).foreach { _.isMax = false }
    PBListsBox.moveView.table.model.asInstanceOf[PBTableModel[PBListItem]].fireTableDataChanged
    PBListsBox.skillView.table.model.asInstanceOf[PBTableModel[PBListItem]].fireTableDataChanged
  }
}

sealed abstract class CatClass (
  val classColor: Color,
  val defaultMove: SupportMoves,
  val secondaryMoves: List[SupportMoves],
  val defaultSkills: List[Skills]) 
case object Charisma extends CatClass (
  Color.YELLOW,
  Palico_Rally,
  List(null),
  List(Slacker_Slap, Last_Stand))
case object Fighting extends CatClass (
  Color.RED,
  Furr_ious,
  List(Demon_Horn, Piercing_Boomerang),
  List(Attack_Up_S, Handicraft))
case object Protection extends CatClass (
  Color.BLUE,
  Taunt,
  List(Emergency_Retreat, Armor_Horn),
  List(Guard_S, Guard_Boost))
case object Assisting extends CatClass (
  Color.MAGENTA,
  Poison_Purr_ison,
  List(Cheer_Horn, Emergency_Retreat),
  List(Monsteradar, Pro_Trapper))
case object Healing extends CatClass (
  Color.GREEN,
  True_Health_Horn,
  List(Armor_Horn, Cheer_Horn),
  List(Defense_Up_S, Health_Harmonics))
case object Bombing extends CatClass (
  Color.ORANGE,
  Mega_Barrel_Bombay,
  List(Camouflage, Demon_Horn),
  List(Heat_Bomb_Res, Bombay_Boost))
case object Gathering extends CatClass (
  Color.WHITE,
  Plunderang,
  List(Piercing_Boomerang, Camouflage),
  List(Gathering_Pro, Pilfer_Boost))

sealed abstract class PBListItem ( 
  var rates: List[Double] = List(1.0,1.0,1.0,1.0),
  val cost: Int = 0,
  val desc: String = "No description"){
  var selected: Boolean = true
  var isMax: Boolean = false
    def bhernaRate = rates(0)
    def kokotoRate = rates(1)
    def pokkeRate = rates(2)
    def yukumoRate = rates(3)
}
case object NullItem extends PBListItem

sealed abstract class SupportMoves(list: List[Double] = List(1,1,1,1), cost: Int = 0) extends PBListItem(list, cost) {
  var moveType: String = ""
}
sealed trait DefaultMoves extends SupportMoves {}
case object Palico_Rally extends DefaultMoves
case object Furr_ious extends DefaultMoves
case object Taunt extends DefaultMoves
case object Poison_Purr_ison extends DefaultMoves
case object True_Health_Horn extends DefaultMoves
case object Mega_Barrel_Bombay extends DefaultMoves
case object Plunderang extends DefaultMoves
case object Mini_Barrel_Bombay extends DefaultMoves
case object Herb_Horn extends DefaultMoves

sealed trait SecondaryMoves extends SupportMoves 
case object Demon_Horn extends SecondaryMoves
case object Piercing_Boomerang extends SecondaryMoves
case object Emergency_Retreat extends SecondaryMoves
case object Armor_Horn extends SecondaryMoves
case object Cheer_Horn extends SecondaryMoves
case object Camouflage extends SecondaryMoves

sealed abstract class MoveGroupA(r1: Int, r2: Int, r3: Int, r4: Int) extends SupportMoves(List(r1/100.0,r2/100.0,r3/100.0,r4/100.0), 3) {}
case object Health_Horn extends MoveGroupA(15,1,5,15)
case object Anti_Monster_Mine_Plus extends MoveGroupA(15,25,5,5)
case object Pilfer extends MoveGroupA(10,5,5,20)
case object Pitfall_Purrison extends MoveGroupA(15,5,15,25)
case object Shock_Purrison extends MoveGroupA(15,5,15,25)
case object Giga_Barrel_Bombay extends MoveGroupA(15,25,5,5)
case object Rath_of_Meow extends MoveGroupA(15,25,5,5)

sealed abstract class MoveGroupB(r1: Int, r2: Int, r3: Int, r4: Int) extends SupportMoves(List(r1/100.0,r2/100.0,r3/100.0,r4/100.0), 2) {}
case object Claw_Dance extends MoveGroupB(10,18,3,3)
case object Weapon_Upgrade extends MoveGroupB(10,13,3,3)
case object Trampoliner extends MoveGroupB(10,4,10,25)
case object Go_Fight_Win extends MoveGroupB(10,4,10,25)
case object Detox_Horn extends MoveGroupB(10,4,30,10)
case object Vase_of_Vitality extends MoveGroupB(10,4,30,10)
case object Mega_Boomerang extends MoveGroupB(10,13,3,3)
case object Flash_Bombay extends MoveGroupB(10,4,5,15)
case object Big_Barrel_Bombay extends MoveGroupB(10,18,3,3)
case object Anti_Monster_Mine extends MoveGroupB(10,18,3,3)

sealed abstract class MoveGroupC(r1: Int, r2: Int, r3: Int, r4: Int) extends SupportMoves(List(r1/100.0,r2/100.0,r3/100.0,r4/100.0), 1) {}
case object Sumo_Stomp extends MoveGroupC(8,10,4,4)
case object Felyne_Comet extends MoveGroupC(8,10,4,4)
case object Dung_Bombay extends MoveGroupC(7,4,15,20)
case object Ultrasonic_Horn extends MoveGroupC(8,4,10,25)
case object Soothing_Roll extends MoveGroupC(8,4,15,4)
case object Parting_Gift extends MoveGroupC(8,4,24,4)
case object Excavator extends MoveGroupC(7,10,4,4)
case object Big_Boomerang extends MoveGroupC(8,10,4,4)
case object Shock_Tripper extends MoveGroupC(8,4,4,15)
case object Chestnut_Cannon extends MoveGroupC(8,10,4,4)
case object Barrel_Bombay extends MoveGroupC(8,10,4,4)
case object Bounce_Bombay extends MoveGroupC(7,10,4,4)
case object Explosive_Roll extends MoveGroupC(7,10,4,4)

sealed abstract class Skills(list: List[Double] = List(1,1,1,1), cost: Int = 0) extends PBListItem(list, cost) {
  var slots: Int = 0
  var color: Color = Color.BLACK 
}
sealed trait DefaultSkills extends Skills {}
case object Slacker_Slap extends DefaultSkills
case object Last_Stand extends DefaultSkills
case object Attack_Up_S extends DefaultSkills
case object Handicraft extends DefaultSkills
case object Guard_S extends DefaultSkills
case object Guard_Boost extends DefaultSkills
case object Monsteradar extends DefaultSkills
case object Pro_Trapper extends DefaultSkills
case object Defense_Up_S extends DefaultSkills
case object Health_Harmonics extends DefaultSkills
case object Heat_Bomb_Res extends DefaultSkills
case object Bombay_Boost extends DefaultSkills
case object Gathering_Pro extends DefaultSkills
case object Pilfer_Boost extends DefaultSkills

sealed abstract class SkillGroupA(r1: Int, r2: Int, r3: Int, r4: Int) extends Skills(List(r1/100.0,r2/100.0,r3/100.0,r4/100.0), 3) {}
case object Element_Attack_Up extends SkillGroupA(15,25,5,5)
case object Status_Attack_Up extends SkillGroupA(10,25,5,10)
case object Anger_Prone extends SkillGroupA(15,25,5,5)
case object Revival_Pro extends SkillGroupA(15,5,30,15)
case object Omniresistance extends SkillGroupA(15,5,25,5)
case object Support_Priority extends SkillGroupA(15,5,15,35)
case object Support_Move_1 extends SkillGroupA(15,10,15,25)

sealed abstract class SkillGroupB(r1: Int, r2: Int, r3: Int, r4: Int) extends Skills(List(r1/100.0,r2/100.0,r3/100.0,r4/100.0), 2) {}
case object Attack_Up_L extends SkillGroupB(10,20,5,5)
case object Critical_Up_L extends SkillGroupB(8,15,5,5)
case object Defense_Up_L extends SkillGroupB(10,5,25,5)
case object Health_Up_L extends SkillGroupB(10,5,20,5)
case object Nine_Lives_Attack extends SkillGroupB(8,15,5,5)
case object Guard_L extends SkillGroupB(10,5,5,20)
case object Knockout_King extends SkillGroupB(8,5,5,20)
case object Earplugs extends SkillGroupB(8,5,10,5)
case object Negate_Stun extends SkillGroupB(8,5,15,5)
case object Counter_Boost extends SkillGroupB(10,15,5,5)
case object Support_Boost extends SkillGroupB(10,5,5,20)

sealed abstract class SkillGroupC(r1: Int, r2: Int, r3: Int, r4: Int) extends Skills(List(r1/100.0,r2/100.0,r3/100.0,r4/100.0), 1) {}
case object Critical_Up_S extends SkillGroupC(6,20,3,4)
case object Health_Up_S extends SkillGroupC(8,6,8,16)
case object Nine_Lives_Defense extends SkillGroupC(6,4,8,10)
case object Boomerang_Pro extends SkillGroupC(6,15,3,4)
case object Stamina_Drain extends SkillGroupC(6,15,3,10)
case object Non_Stick_Fur extends SkillGroupC(6,4,8,15)
case object Negate_Wind extends SkillGroupC(8,4,8,4)
case object Negate_Sleep extends SkillGroupC(6,4,8,4)
case object Iron_Hide extends SkillGroupC(6,4,8,4)
case object Negate_Paralysis extends SkillGroupC(6,4,8,4)
case object Tremor_Res extends SkillGroupC(6,4,8,4)
case object Negate_Poison extends SkillGroupC(8,4,8,4)
case object Biology extends SkillGroupC(6,4,8,4)
case object Negate_Confusion extends SkillGroupC(8,4,8,4)
case object Goldenfish_Catcher extends SkillGroupC(8,4,3,10)
