extern HTStream * HTSaveToFile PARAMS((
        HTPresentation *        pres,
        HTParentAnchor *        anchor, /* Not used */
        HTStream *              sink));

extern HTStream * HTDumpToStdout PARAMS((
        HTPresentation *        pres,
        HTParentAnchor *        anchor, /* Not used */
        HTStream *              sink));

