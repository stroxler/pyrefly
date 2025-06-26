use pyrefly_util::lined_buffer::UserRange;
use ruff_source_file::LineColumn;
use ruff_text_size::TextSize;

use crate::module::module_info::ModuleInfo;

pub fn user_range_to_range(x: &UserRange) -> lsp_types::Range {
    lsp_types::Range::new(
        source_location_to_position(&x.start),
        source_location_to_position(&x.end),
    )
}

fn source_location_to_position(x: &LineColumn) -> lsp_types::Position {
    lsp_types::Position {
        line: x.line.to_zero_indexed() as u32,
        character: x.column.to_zero_indexed() as u32,
    }
}

pub fn text_size_to_position(info: &ModuleInfo, x: TextSize) -> lsp_types::Position {
    source_location_to_position(&info.line_column(x))
}

pub fn position_to_text_size(info: &ModuleInfo, position: lsp_types::Position) -> TextSize {
    info.lined_buffer()
        .to_text_size(position.line, position.character)
}
