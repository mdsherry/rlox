use miette::SourceSpan;

pub trait HasSpan {
    fn span(&self) -> SourceSpan;
}

impl HasSpan for SourceSpan {
    fn span(&self) -> SourceSpan {
        self.clone()
    }
}

impl<T> HasSpan for &T where T: HasSpan {
     fn span(&self) -> SourceSpan {
        (*self).span()
    }
}
impl<T> HasSpan for &mut T where T: HasSpan {
    fn span(&self) -> SourceSpan {
       (*self).span()
   }
}

pub fn join_spans<L: HasSpan, R: HasSpan>(left: L, right: R) -> SourceSpan {
    glue_spans(left.span(), right.span())
}

fn glue_spans(left: SourceSpan, right: SourceSpan) -> SourceSpan {
    let start = left.offset();
    let end = right.offset() + right.len();
    (start..end).into()
}

