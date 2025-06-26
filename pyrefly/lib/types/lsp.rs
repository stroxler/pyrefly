use pyrefly_util::lined_buffer::LinedBuffer;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

pub fn text_range_to_range(info: &LinedBuffer, x: TextRange) -> lsp_types::Range {
    lsp_types::Range::new(
        text_size_to_position(info, x.start()),
        text_size_to_position(info, x.end()),
    )
}

pub fn text_size_to_position(info: &LinedBuffer, x: TextSize) -> lsp_types::Position {
    let user_pos = info.user_pos(x);
    lsp_types::Position {
        line: user_pos.line.to_zero_indexed() as u32,
        character: user_pos.column.to_zero_indexed() as u32,
    }
}

pub fn position_to_text_size(info: &LinedBuffer, position: lsp_types::Position) -> TextSize {
    info.to_text_size(position.line, position.character)
}

pub fn range_to_text_range(info: &LinedBuffer, position: lsp_types::Range) -> TextRange {
    TextRange::new(
        position_to_text_size(info, position.start),
        position_to_text_size(info, position.end),
    )
}
