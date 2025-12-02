// Helper functions for managing tasks with both jj and discovery tree
import * as jj from '~/.claude/skills/using-jj/src/jj.js';
import { setWorkspace, updateTask, closeTask, findReadyTasks, drawTree } from '~/.claude/skills/discovery-tree-workflow/src/beads.js';

const REPO_PATH = '/Users/zell/play/advent-of-code-2025';

export async function startTask(description: string, taskTitlePattern: string) {
  // Set contexts
  await jj.setRepository({ repositoryPath: REPO_PATH });
  await setWorkspace({ workspacePath: REPO_PATH });

  // Start jj task FIRST
  await jj.startTask({ description });

  // Then find and mark discovery tree task as in_progress
  const readyTasks = await findReadyTasks({ limit: 10 });
  const task = readyTasks.find(t => t.title.includes(taskTitlePattern));

  if (task) {
    await updateTask({
      taskId: task.id,
      status: "in_progress"
    });
    return task.id;
  }

  throw new Error(`Task with pattern "${taskTitlePattern}" not found`);
}

export async function finishTask(taskId: string, rootTaskId: string, completionNote: string) {
  // Set contexts
  await jj.setRepository({ repositoryPath: REPO_PATH });
  await setWorkspace({ workspacePath: REPO_PATH });

  // Close discovery tree task FIRST
  await closeTask({
    taskId,
    reason: 'Completed'
  });

  // Update root task with progress
  const rootTask = await updateTask({
    taskId: rootTaskId,
    notes: completionNote
  });

  // THEN finish jj task (which creates new empty working copy above the beads changes)
  await jj.finishTask();

  return rootTask;
}

export async function checkpoint(summary: string) {
  await jj.setRepository({ repositoryPath: REPO_PATH });
  await jj.checkpoint({ summary });
}

export async function showTree(epicId: string) {
  await setWorkspace({ workspacePath: REPO_PATH });
  const tree = await drawTree({ taskId: epicId });
  return tree;
}
