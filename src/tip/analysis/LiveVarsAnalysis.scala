package tip.analysis

import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}
import tip.ast.AstOps.AstOp
import tip.ast._
import tip.cfg._
import tip.lattices._
import tip.solvers._

/**
  * Base class for live variables analysis.
  */
abstract class LiveVarsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(false) {

  val lattice: MapLattice[CfgNode, PowersetLattice[ADeclaration]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => Set.empty //lattice.sublattice.bottom - type error in IDE
      case r: CfgStmtNode =>
        r.data match {
          //add all vars in condition to live vars
          case cond: AExpr => s union cond.appearingIds
          case as: AAssignStmt =>
            as.left match {
              //remove id from right part, add all ids from left part
              case id: AIdentifier => (s - id.declaration) union as.right.appearingIds
              case _ => ???
            }
          //remove all declared vars
          case varr: AVarStmt => s -- varr.declIds
          //add all vars in return expression
          case ret: AReturnStmt => s union ret.exp.appearingIds
          //add all vars in output expression
          case out: AOutputStmt => s union out.exp.appearingIds
          case _ => s
        }
      case _ => s
    }
}

/**
  * Live variables analysis that uses the simple fixpoint solver.
  */
class LiveVarsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies

/**
  * Live variables analysis that uses the worklist solver.
  */
class LiveVarsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with BackwardDependencies
