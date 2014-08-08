CREATE TABLE users
(
  username text NOT NULL,
  cur_profile_pic text,
  CONSTRAINT pk_username PRIMARY KEY (username)
)
WITH (
  OIDS=FALSE
);
ALTER TABLE users
  OWNER TO postgres;