import { describe, it, expect, vi, beforeEach } from 'vitest';
import { generateSymbolSVG } from './signWritingFont';
import ssw from './utils/SuttonSignWriting';

// Mock the SSW module
vi.mock('./utils/SuttonSignWriting', () => ({
  default: {
    pua: vi.fn(() => '󽠰'),
    code: vi.fn(() => '󽠰'), 
    size: vi.fn(() => '76,76'),
    svg: vi.fn(),
    symbol: vi.fn(),
    fsw: vi.fn(),
    colorize: vi.fn(),
    sizes: {}
  }
}));

describe('SignWriting Dual-Font System', () => {
  beforeEach(() => {
    // Reset mocks and set up default dual-font SVG return value
    vi.clearAllMocks();
    (ssw.svg as any).mockReturnValue(`
      <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="30" height="30" viewBox="0 0 30 30">
        <text style="font-size:0%;">S10000</text>
        <g transform="translate(15,15)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:#ffffff;">󽠰</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWriting';font-size:30px;fill:#000000;">󽠰</text>
        </g>
      </svg>
    `.trim());
  });
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
    expect(svg).toContain('fill:#ffffff');
    expect(svg).toContain('fill:#000000');
  });
  it('handles custom colors', async () => {
    // Set up mock for custom colors
    (ssw.svg as any).mockReturnValue(`
      <svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="40" height="40" viewBox="0 0 40 40">
        <text style="font-size:0%;">S10000</text>
        <g transform="translate(20,20)">
          <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:#ff0000;">󽠰</text>
          <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWriting';font-size:30px;fill:#0000ff;">󽠰</text>
        </g>
      </svg>
    `.trim());

    const symbol = { 
      key: 'S10000',
      nwcolor: '#ff0000',  // Custom fill color
      nbcolor: '#0000ff'   // Custom line color
    };
    
    const svg = await generateSymbolSVG(symbol);
    expect(svg).toContain('fill:#ff0000');
    expect(svg).toContain('fill:#0000ff');
  });
});
