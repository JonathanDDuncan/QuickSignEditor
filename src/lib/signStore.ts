import { writable, type Writable } from 'svelte/store';
import type { SignData } from './utils/types';
import { createHistory, undo, redo, record } from './utils/history';
import chooserData from '../../legacy/chooserclassification.json';

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

// Fix type error: explicitly type the store to allow any[] for maniquinchoosings
export const signModel = writable<{ maniquinchoosings: any[] }>({ maniquinchoosings: [] });

export function loadManiquinData() {
  // Example: use the first 5 items for the maniquin
  const maniquinchoosings = chooserData.basechooseritems.slice(0, 5).map((item, i) => ({
    displaySign: {
      svg: `<svg width="40" height="40"><rect x="0" y="0" width="40" height="40" fill="#e6f0fa" stroke="#0078d4"/><text x="5" y="20" font-size="10">${item.name}</text></svg>`,
      width: 40,
      height: 40,
      label: item.name
    },
    offset: { offsetx: i * 45, offsety: 0 }
  }));

  signModel.set({ maniquinchoosings });
}
