import { writable, type Writable } from 'svelte/store';
import type { SignData } from './utils/types';
import { createHistory, undo, redo, record } from './utils/history';

// Initial sign data (empty sign)
const initialSignData: SignData = {
  name: 'Demo Sign',
  symbols: [
    { id: 'S1001', x: 50, y: 60, selected: false },
    { id: 'S1002', x: 120, y: 100, selected: false }
  ],
  metadata: {}
};

const initialHistory = createHistory(initialSignData);
const { subscribe, set, update } = writable(initialHistory);

export const signStore = {
  subscribe,
  set: (signData: SignData) => set(createHistory(signData)),
  update: (fn: (sd: SignData) => SignData) => {
    update(h => record(h, fn(h.present)));
  },
  undo: () => update(h => undo(h)),
  redo: () => update(h => redo(h)),
  canUndo: () => {
    let can = false;
    subscribe(h => { can = h.past.length > 0; })();
    return can;
  },
  canRedo: () => {
    let can = false;
    subscribe(h => { can = h.future.length > 0; })();
    return can;
  },
  getPresent: () => {
    let present: SignData = initialSignData;
    subscribe(h => { present = h.present; })();
    return present;
  }
};
