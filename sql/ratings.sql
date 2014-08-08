CREATE TABLE ratings
(
  rater text NOT NULL,
  ratee text NOT NULL,
  is_hot boolean,
  CONSTRAINT pk_rater_ratee PRIMARY KEY (rater, ratee),
  CONSTRAINT fk_ratings_users_ratee FOREIGN KEY (ratee)
      REFERENCES users (username) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION,
  CONSTRAINT fk_ratings_users_rater FOREIGN KEY (rater)
      REFERENCES users (username) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE NO ACTION
)
WITH (
  OIDS=FALSE
);
ALTER TABLE ratings
  OWNER TO postgres;
