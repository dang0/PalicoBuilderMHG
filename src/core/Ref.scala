package core

import java.awt.Dimension

object Ref {
  private def Ref() {}
  final val MAJ_VERSION = 1
  final val MIN_VERSION = 2
  final val REV_VERSION = 4
  final val BLD_VERSION = 1
  final val VERSION_STR = MAJ_VERSION + "." + MIN_VERSION + "." + REV_VERSION + "." + BLD_VERSION
  final val VERSION_vSTR = "v" + VERSION_STR
  final val PROGRAM_NAME = "[MHG] Palico Builder"
  final val MAIN_TITLE = PROGRAM_NAME + " " + VERSION_vSTR
  final val MAIN_SIZE = new Dimension(600,450)
  final val USELESS_STR = "thanks for reading my code :)"
}