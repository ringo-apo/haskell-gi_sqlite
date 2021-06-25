# my-project
# データベース作成

```
sqlite3 test.db
CREATE TABLE memos (id INTEGER PRIMARY KEY AUTOINCREMENT, comment);
insert into memos (comment) values ('Hello');
insert into memos (comment) values ('Konitiwa');
select * from memos;
.exit
```

# コンパイル＆実行

```
stack build
stack exec my-project-exe
```
