# Workflow Notes

## Task Management Order

**CRITICAL**: When managing tasks with both jj and discovery tree, follow this exact order:

1. **Start jj task FIRST**: `jj.startTask({ description })`
2. **Start discovery tree task**: `updateTask({ taskId, status: "in_progress" })`
3. **Do work and checkpoint as needed**: `jj.checkpoint({ summary })`
4. **Close discovery tree task FIRST**: `closeTask({ taskId, reason: 'Completed' })`
5. **Update root task**: `updateTask({ taskId: rootTaskId, notes })`
6. **Finish jj task LAST**: `jj.finishTask()`

### Why This Order Matters

- Discovery tree operations modify `.beads/issues.jsonl`
- `jj.finishTask()` creates a new empty working copy above the parent commit
- If you finish jj task before closing discovery tree tasks, the beads changes end up in an undescribed working copy
- Always close discovery tree tasks BEFORE calling `jj.finishTask()`

## Running Cargo Commands

Since the Rust toolchain is provided by Nix flake:
- Either run commands in `nix develop` shell
- Or use `nix develop --command <command>`
- Or let direnv automatically load the environment
