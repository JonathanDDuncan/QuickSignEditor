/**
 * @fileoverview Pure history management functions for undo/redo
 * All functions are under 20 lines and side-effect free
 */

/**
 * Adds an action to the undo history
 * @param {Array} undoHistory - Current undo history array
 * @param {Object} state - Current state to save
 * @param {string} actionName - Name of the action
 * @param {number} maxHistory - Maximum history items to keep
 * @returns {Array} New undo history array
 */
export function addToUndoHistory(undoHistory, state, actionName, maxHistory = 50) {
  if (!undoHistory || !Array.isArray(undoHistory)) {
    undoHistory = [];
  }
  
  const newHistoryItem = {
    action: actionName,
    state: JSON.parse(JSON.stringify(state)), // Deep copy
    timestamp: Date.now()
  };
  
  const newHistory = [newHistoryItem, ...undoHistory];
  return newHistory.slice(0, maxHistory);
}

/**
 * Gets the previous state from undo history
 * @param {Array} undoHistory - Current undo history array
 * @returns {{state: Object|null, newHistory: Array}} Previous state and updated history
 */
export function undoFromHistory(undoHistory) {
  if (!undoHistory || undoHistory.length === 0) {
    return { state: null, newHistory: [] };
  }
  
  const [previousState, ...remainingHistory] = undoHistory;
  return {
    state: previousState.state,
    newHistory: remainingHistory
  };
}

/**
 * Checks if undo is available
 * @param {Array} undoHistory - Current undo history array
 * @returns {boolean} True if undo is available
 */
export function canUndo(undoHistory) {
  return !!(undoHistory && Array.isArray(undoHistory) && undoHistory.length > 0);
}
