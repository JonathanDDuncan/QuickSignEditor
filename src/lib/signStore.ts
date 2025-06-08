import { writable, type Writable } from 'svelte/store';
import type { SignData } from './utils/types';

// Initial sign data (empty sign)
const initialSignData: SignData = {
  name: '',
  symbols: [],
  metadata: {}
};

const store: Writable<SignData> = writable<SignData>(initialSignData);
export const signStore = store;
