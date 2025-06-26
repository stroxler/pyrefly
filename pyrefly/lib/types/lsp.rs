use pyrefly_util::lined_buffer::LinedBuffer;
use pyrefly_util::lined_buffer::UserPos;
use pyrefly_util::lined_buffer::UserRange;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

pub fn user_range_to_range(x: &UserRange) -> lsp_types::Range {
    lsp_types::Range::new(
        source_location_to_position(&x.start),
        source_location_to_position(&x.end),
    )
}

fn source_location_to_position(x: &UserPos) -> lsp_types::Position {
    lsp_types::Position {
        line: x.line.to_zero_indexed() as u32,
        character: x.column.to_zero_indexed() as u32,
    }
}

pub fn text_size_to_position(info: &LinedBuffer, x: TextSize) -> lsp_types::Position {
    source_location_to_position(&info.user_pos(x))
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
