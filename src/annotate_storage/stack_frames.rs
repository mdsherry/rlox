use std::collections::HashMap;

use crate::grammar::StrSymbol;

#[derive(Debug, Clone)]
pub struct Frame<T, S> {
    symbols: HashMap<StrSymbol, T>,
    pub frame_state: S,
    pub func_boundary: bool,
}

impl<T, S> Frame<T, S> {
    fn new(frame_state: S) -> Self {
        Frame {
            symbols: HashMap::new(),
            frame_state,
            func_boundary: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentStack<T, S> {
    pub frames: Vec<Frame<T, S>>,
}
impl<T, S> IdentStack<T, S> {
    pub fn new(initial_state: S) -> Self {
        IdentStack {
            frames: vec![Frame::new(initial_state)],
        }
    }

    pub fn push_frame(&mut self, frame_state: S) {
        self.frames.push(Frame::new(frame_state));
    }

    pub fn pop_frame(&mut self) -> Frame<T, S> {
        self.frames
            .pop()
            .expect("Popped more frames than were pushed")
    }

    pub fn get_symbol(&self, symbol: StrSymbol) -> Option<(&T, bool)> {
        let mut crossed_func_boundary = false;
        for frame in self.frames.iter().rev() {
            if let Some(data) = frame.symbols.get(&symbol) {
                return Some((data, crossed_func_boundary));
            }
            crossed_func_boundary |= frame.func_boundary;
        }
        None
    }

    pub fn add_symbol(&mut self, symbol: StrSymbol, data: T) {
        let frame = self
            .frames
            .last_mut()
            .expect("There should always be at least one frame");
        frame.symbols.insert(symbol, data);
    }

    pub fn last(&self) -> &Frame<T, S> {
        self.frames.last().expect("Always at least one frame")
    }
    pub fn last_mut(&mut self) -> &mut Frame<T, S> {
        self.frames.last_mut().expect("Always at least one frame")
    }
}
