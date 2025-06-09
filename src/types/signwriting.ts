// SignWriting type definitions
// Global type definitions for the SuttonSignWriting.js library

declare global {
  interface Window {
    ssw: {
      svg: (text: string, options?: any) => string;
      size: (text: string) => string;
      key: (text: string, style?: boolean) => string;
      fsw: (text: string, style?: boolean) => string;
      colorize: (key: string) => string;
      code: (text: string, hexval?: boolean) => string;
      pua: (text: string, hexval?: boolean) => string;
      uni: (text: string, type?: string) => string;
      bbox: (text: string) => string;
      symbol: (text: string) => string;
      parse: (text: string) => any;
      sign: (text: string, style?: boolean) => string;
      chars: (text: string) => string;
    };
  }
}

export interface SignWritingSymbol {
  key: string;
  unicodepua?: string;
  unicode?: string;
  name?: string;
  description?: string;
  category?: string;
  group?: string;
  fill?: number;
  rotation?: number;
  flip?: boolean;
  size?: number;
  width?: number;
  height?: number;
  nwcolor?: string; // fill color (white)
  nbcolor?: string; // line color (black)
}

export interface SignWritingSign {
  fsw: string;
  symbols: SignWritingSymbol[];
  spatials?: string[];
  style?: string;
  zoom?: number;
}

export interface SignWritingOptions {
  style?: string;
  size?: number;
  colorize?: boolean;
  background?: string;
  padding?: number;
  zoom?: number;
}
