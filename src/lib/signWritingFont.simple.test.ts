import { describe, it, expect, vi } from 'vitest';
import { generateSymbolSVG } from './signWritingFont';

// Mock the global window object for browser environment
Object.defineProperty(globalThis, 'window', {
  value: {
    ssw: {
      pua: vi.fn(() => '󽠰'),
      code: vi.fn(() => '󽠰'), // Add proper mock for code function
      size: vi.fn(() => '76,76'),
      svg: vi.fn(),
      key: vi.fn(),
      fsw: vi.fn(),
      colorize: vi.fn(),
      sizes: {}
    }
  },
  writable: true
});

describe('SignWriting Dual-Font System', () => {
  it('generates SVG with both fill and line fonts', async () => {
    const symbol = {
      key: 'S10000',
      width: 40,
      height: 40,
      size: 1
    };

    const svg = await generateSymbolSVG(symbol, { size: 30 });

    // Verify dual-font structure with official class names
    expect(svg).toContain('class="sym-fill"');
    expect(svg).toContain('class="sym-line"');
    expect(svg).toContain('SuttonSignWritingFill');
    expect(svg).toContain('SuttonSignWriting');
    
    // Both text elements should contain Unicode characters
    const unicodeMatches = svg.match(/󽠰/g);
    expect(unicodeMatches).toHaveLength(2);
  });

  it('applies correct colors to fonts', async () => {
    const symbol = { key: 'S10000' };
    const svg = await generateSymbolSVG(symbol);

    // Default colors: white fill, black line
    expect(svg).toContain('fill: #ffffff');
    expect(svg).toContain('fill: #000000');
  });

  it('handles custom colors', async () => {
    const symbol = { 
      key: 'S10000',
      nwcolor: '#ff0000',  // Custom fill color
      nbcolor: '#0000ff'   // Custom line color
    };
    
    const svg = await generateSymbolSVG(symbol);
    expect(svg).toContain('fill: #ff0000');
    expect(svg).toContain('fill: #0000ff');
  });
});
