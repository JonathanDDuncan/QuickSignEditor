import { describe, it, expect, vi, beforeEach } from 'vitest';
import { generateFSWSVG } from './signWritingFont';
import ssw from './utils/SuttonSignWriting';

// Mock the SSW module
vi.mock('./utils/SuttonSignWriting', () => {
  return {
    default: {
      svg: vi.fn(),
      size: vi.fn(() => '76x76'),
      symbol: vi.fn(),
      parse: vi.fn()
    }
  };
});

describe('SignWriting FSW Dual-Font System', () => {
  beforeEach(() => {
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
    
    (ssw.svg as any).mockReturnValue(mockOfficialSVG);

    const result = await generateFSWSVG(fswString);

    // Should contain SVG structure
    expect(result).toContain('<svg');
    expect(result).toContain('xmlns="http://www.w3.org/2000/svg"');
  });

  it('should handle FSW strings with positioning', async () => {
    const fswString = "M521x547S33100482x483S20310506x500";
    
    (ssw.svg as any).mockReturnValue('<svg width="521" height="547" viewBox="0 0 521 547" xmlns="http://www.w3.org/2000/svg"></svg>');
    
    const result = await generateFSWSVG(fswString);
    
    // Should call svg function with the right FSW string
    expect(ssw.svg).toHaveBeenCalledWith(fswString, expect.any(Object));
  });

  it('should handle empty or invalid FSW strings', async () => {
    // Test empty string
    let error;
    try {
      await generateFSWSVG('');
    } catch (e) {
      error = e;
    }
    expect(error).toBeDefined();
    expect((error as Error).message).toContain('No FSW string provided');
    
    // Test null
    error = undefined;
    try {
      await generateFSWSVG(null as any);
    } catch (e) {
      error = e;
    }
    expect(error).toBeDefined();
    expect((error as Error).message).toContain('No FSW string provided');
  });

  it('should apply custom colors to dual-font layers', async () => {
    const fswString = "S10000482x483";
    
    const customColoredSVG = `<svg version="1.1" xmlns="http://www.w3.org/2000/svg" width="36" height="34" viewBox="482 483 36 34">
      <text style="font-size:0%;">S10000482x483</text>
      <g transform="translate(482,483)">
        <text class="sym-fill" style="pointer-events:none;font-family:'SuttonSignWritingFill';font-size:30px;fill:#ff0000;">􀀁</text>
        <text class="sym-line" style="pointer-events:none;font-family:'SuttonSignWritingLine';font-size:30px;fill:#0000ff;">󰀁</text>
      </g>
    </svg>`;
    
    (ssw.svg as any).mockReturnValue(customColoredSVG);
    
    const result = await generateFSWSVG(fswString, {
      fill: '#ff0000',
      line: '#0000ff'
    });
    
    expect(result).toContain('#ff0000');
    expect(result).toContain('#0000ff');
  });
});
