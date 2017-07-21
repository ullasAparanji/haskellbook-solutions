newtype Reader r a = Reader {runReader :: r -> a}

asks
