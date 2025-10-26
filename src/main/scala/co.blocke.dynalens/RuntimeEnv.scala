package co.blocke.dynalens

final case class RuntimeEnv(
                             biMapRegistry: _BiMapRegistry
                             // future: db: Database, http: HttpClient, logger: Logger, ...
                           )