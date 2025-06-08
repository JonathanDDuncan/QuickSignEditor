import { describe, it, expect, vi, beforeEach } from 'vitest';
import { generateFSWSVG } from './signWritingFont';

// Mock window.ssw
const mockSsw = {
  svg: vi.fn(),
  size: vi.fn(() => '76x76'),
  pua: vi.fn(() => '󽠰'),
  code: vi.fn(() => '󽠰')
};

describe('SignWriting FSW Dual-Font System', () => {
  beforeEach(() => {
    // Mock window object
    Object.defineProperty(global, 'window', {
      value: {
        ssw: mockSsw
      },
      writable: true
    });

    // Reset mocks
    vi.clearAllMocks();
  });

  it('should generate dual-font SVG for complete FSW strings', async () => {
    const fswString = "AS20310S26b02S33100M521x547S33100482x483S20310506x500S26b02503x520";
    
    // Mock the official SVG output
    const mockOfficialSVG = `<svg width="521" height="547" viewBox="0 0 521 547" xmlns="http://www.w3.org/2000/svg">
      <text x="482" y="483" style="font-family:'SuttonSignWritingLine';">󽠰</text>
      <text x="506" y="500" style="font-family:'SuttonSignWritingLine';">󽠱</text>
      <text x="503" y="520" style="font-family:'SuttonSignWritingLine';">󽠲</text>
    </svg>`;
    
    mockSsw.svg.mockReturnValue(mockOfficialSVG);
    mockSsw.pua.mockReturnValue('󽠰');
    mockSsw.code.mockReturnValue('󽠰');

    const result = await generateFSWSVG(fswString);

    // Should contain dual-font structure
    expect(result).toContain('<svg');
    expect(result).toContain('sym-fill');
    expect(result).toContain('sym-line');
    expect(result).toContain('SuttonSignWritingFill');
    expect(result).toContain('SuttonSignWriting');
    
    // Should process the symbols
    expect(mockSsw.pua).toHaveBeenCalled();
    expect(mockSsw.code).toHaveBeenCalled();
  });

  it('should handle FSW strings with positioning', async () => {
    const fswString = "M521x547S33100482x483S20310506x500";
    
    mockSsw.svg.mockReturnValue('<svg width="521" height="547" viewBox="0 0 521 547" xmlns="http://www.w3.org/2000/svg"></svg>');
    
    const result = await generateFSWSVG(fswString);
    
    // Should contain transform with positioning
    expect(result).toContain('transform="translate(482,483)"');
    expect(result).toContain('transform="translate(506,500)"');
  });

  it('should fallback to official SVG on error', async () => {
    const fswString = "InvalidFSW";
    const fallbackSVG = '<svg><text>fallback</text></svg>';
    
    mockSsw.svg.mockReturnValue(fallbackSVG);
    // Make pua/code throw errors to trigger fallback
    mockSsw.pua.mockImplementation(() => { throw new Error('Invalid symbol'); });
    mockSsw.code.mockImplementation(() => { throw new Error('Invalid symbol'); });

    const result = await generateFSWSVG(fswString);
    
    expect(result).toBe(fallbackSVG);
  });

  it('should handle empty or invalid FSW strings', async () => {
    await expect(generateFSWSVG('')).rejects.toThrow('No FSW string provided');
    await expect(generateFSWSVG(null as any)).rejects.toThrow('No FSW string provided');
  });

  it('should apply custom colors to dual-font layers', async () => {
    const fswString = "S10000482x483";
    
    mockSsw.svg.mockReturnValue('<svg width="100" height="100" viewBox="0 0 100 100"></svg>');
    
    const result = await generateFSWSVG(fswString, {
      fill: '#ff0000',
      line: '#0000ff'
    });
    
    expect(result).toContain('fill: #ff0000');
    expect(result).toContain('fill: #0000ff');
  });
});
