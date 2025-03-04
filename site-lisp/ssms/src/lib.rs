extern crate emacs;
extern crate odbc_api;

use emacs::{defun, Env, IntoLisp, Result, Value, Vector};
use lazy_static::lazy_static;
use odbc_api::{buffers::TextRowSet, Connection, ConnectionOptions, Cursor, Environment};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

emacs::define_errors! {
    no_value_error "This Job number is not present" (arith_error range_error)
}

#[emacs::module(name = "ssms-core")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Done loading! 123")
}

#[defun(user_ptr)]
fn connect() -> Result<Connection<'static>> {
    lazy_static! {
        static ref ENV: Environment = Environment::new().unwrap();
    }
    let connection_string = "
        Driver={ODBC Driver 18 for SQL Server};\
        Server=VMWSDB01.gold.local;\
        Database=Goldstein 206;\
        UID=sa;\
        PWD=sql_admin15;\
        Encrypt=no;\
        TrustServerCertificate=yes;\
        ";
    let conn =
        ENV.connect_with_connection_string(connection_string, ConnectionOptions::default())?;
    Ok(conn)
}

fn vec_to_vector<'e, T: IntoLisp<'e>>(env: &'e Env, vec: Vec<T>) -> Result<Vector<'e>> {
    let vector = env.make_vector(vec.len(), ())?;
    for (i, v) in vec.into_iter().enumerate() {
        vector.set(i, v)?;
    }
    Ok(vector)
}

#[defun]
fn get_all_jobs<'a>(env: &'a Env, conn: &Connection) -> Result<Vector<'a>> {
    if let Ok(Some(mut cursor)) = conn.execute(
        "SELECT S_ASJOBNO, S_DESCRIPTION FROM sao.ASJOB WHERE DT_DELETED IS NULL AND I_ASJOB <> 0",
        (),
    ) {
        let mut buffers = TextRowSet::for_cursor(100, &mut cursor, Some(4096))?;
        let mut row_set_cursor = cursor.bind_buffer(&mut buffers)?;
        let mut ids = vec![];
        while let Some(batch) = row_set_cursor.fetch()? {
            for row_index in 0..batch.num_rows() {
                ids.push(
                    std::str::from_utf8(batch.at(0, row_index).unwrap_or(&[]))
                        .unwrap()
                        .replace("\r\n", "\n"),
                );
            }
        }
        return Ok(vec_to_vector(env, ids)?);
    }
    Ok(env.make_vector(0, ())?)
}

#[defun]
fn get_job(env: &Env, conn: &Connection, jobno: String) -> Result<String> {
    let mut cursor = conn
        .execute(
            &format!(
                "SELECT TOP 1 LS_CODE FROM sao.ASJOB WHERE S_ASJOBNO = '{}' AND DT_DELETED IS NULL",
                &jobno
            ),
            (),
        )?
        .expect("Assume select statement creates cursor");
    if let Some(mut row) = cursor.next_row()? {
        let mut buf = Vec::new();
        row.get_text(1, &mut buf)?;
        let ret = String::from_utf8(buf).unwrap().replace("\r\n", "\n");
        Ok(ret)
    } else {
        env.signal(no_value_error, (8,))
    }
}
