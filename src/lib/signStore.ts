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
  console.log('loadManiquinData() called - starting to load data');
  
  // Use the same complex FSW string as the Elm implementation - pass entire FSW to ssw.svg()
  const complexFSW = "M600X697S33000476X489S2FF00484X488S30A50495X490S30A40516X490S31430498X500S33E10501X520S33110510X511S36D10481X558S36A10498X538S37601546X560S37609463X560S14C10554X555S20500576X542S21600552X542S22800463X558S2F700562X519S22A00470X636S22E00583X587S23800487X631S24200506X631S24B00534X628S28800468X668S29900491X663S2E300534X665S2ED00568X587S2F100448X542S2F500564X655S36B10463X454S36C00493X447S38500534X443S37E0F440X559S29600511X663S38700476X695--Z01,1.48Z02,1.51Z05,1.21Z06,1.45Z08,1.63Z09,1.45Z10,1.6Z11,1.6Z25,0.79Z28,0.79Z29,0.79Z30,0.79";
  
  console.log('Using complete FSW string:', complexFSW);
  console.log('FSW length:', complexFSW.length, 'characters');
  
  try {    // Create a single mannequin choosing that contains the entire FSW sign
    const maniquinchoosings = [{
      displaySign: {
        fsw: complexFSW, // Pass the entire FSW string
        completeFSW: true, // Flag to indicate this is a complete FSW
        // Remove width/height constraints to prevent cropping
        label: 'Complete SignWriting Mannequin'
      },
      offset: { 
        offsetx: 0, // No offset needed for complete FSW
        offsety: 0 
      },
      value: complexFSW,
      valuestoAdd: [] // Not needed for complete FSW
    }];

    console.log('Generated single mannequin choosing with complete FSW');
    console.log('Setting signModel with complete FSW sign');

    signModel.set({ maniquinchoosings });
    
    // Verify the data was set
    signModel.subscribe(value => {
      console.log('signModel updated with complete FSW sign');
    })();
    
  } catch (error) {
    console.error('Failed to load complete FSW:', error);
    // Fallback to simple test data
    loadSimpleTestData();
  }
}

// Simple FSW parser to extract symbols and coordinates
function parseFSWString(fsw: string): Array<{key: string, x: number, y: number}> {
  const symbols: Array<{key: string, x: number, y: number}> = [];
  
  // Regular expression to match symbols like "S33000476x489"
  const symbolRegex = /S[0-9a-f]{5}(\d{3})x(\d{3})/gi;
  let match;
  
  console.log('Starting FSW parsing with regex:', symbolRegex);
  
  while ((match = symbolRegex.exec(fsw)) !== null) {
    const key = match[0].substring(0, 6); // Extract S33000 part
    const x = parseInt(match[1]);
    const y = parseInt(match[2]);
    
    console.log(`Parsed symbol: ${key} at position (${x}, ${y})`);
    symbols.push({ key, x, y });
  }
  
  console.log(`Finished parsing FSW, found ${symbols.length} symbols`);
  return symbols;
}

// Fallback simple test data
function loadSimpleTestData() {
  console.log('Loading simple test data as fallback');
  
  const exampleSymbols = [
    { symbolkey: 'S10000', name: 'Index Finger', x: 500, y: 500 },
    { symbolkey: 'S26a00', name: 'Double Movement', x: 570, y: 500 },
    { symbolkey: 'S30100', name: 'Head Movement', x: 640, y: 500 },
    { symbolkey: 'S10001', name: 'Index Finger Variant', x: 710, y: 500 },
    { symbolkey: 'S2f500', name: 'Arrow', x: 780, y: 500 }
  ];

  const maniquinchoosings = exampleSymbols.map((sym, i) => ({
    displaySign: {
      fsw: sym.symbolkey,
      width: 60,
      height: 60,
      label: sym.name
    },
    offset: { offsetx: sym.x - 500, offsety: sym.y - 500 },
    value: sym.symbolkey
  }));

  signModel.set({ maniquinchoosings });
}
