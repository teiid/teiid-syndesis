package org.komodo.rest.relational.json;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;

import java.io.IOException;

import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.RestNamedVdbStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataVdbStatusVdb;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A serializer for {@link RestNamedVdbStatus} objects.
 */
public final class NamedVdbStatusSerializer extends TypeAdapter< RestNamedVdbStatus > {

	/* (non-Javadoc)
	 * @see org.komodo.rest.relational.json.AbstractEntitySerializer#read(com.google.gson.stream.JsonReader)
	 */
	@Override
	public RestNamedVdbStatus read( final JsonReader in ) throws IOException {
		final RestNamedVdbStatus entity = new RestNamedVdbStatus();
		in.beginObject();

		while ( in.hasNext() ) {
			final String name = in.nextName();

			if ( RestNamedVdbStatus.OBJECT_NAME_LABEL.equals( name ) ) {
				entity.setName( in.nextString() );
			} else if ( RestNamedVdbStatus.VDB_STATUS_LABEL.equals( name ) ) {
				final RestMetadataVdbStatusVdb status = BUILDER.fromJson( in, RestMetadataVdbStatusVdb.class );
				entity.setVdbStatus( status );
			} else {
				throw new IOException( Messages.getString( Messages.Error.UNEXPECTED_JSON_TOKEN, name ) );
			}
		}

		in.endObject();
		return entity;
	}

	/* (non-Javadoc)
	 * @see org.komodo.rest.relational.json.AbstractEntitySerializer#write(com.google.gson.stream.JsonWriter, org.komodo.rest.AbstractKEntity)
	 */
	@Override
	public void write( final JsonWriter out,
			final RestNamedVdbStatus entity ) throws IOException {
		out.beginObject();

		// name
		out.name( RestNamedVdbStatus.OBJECT_NAME_LABEL );
		out.value( entity.getName() );

		// VDB status
		out.name( RestNamedVdbStatus.VDB_STATUS_LABEL );

		if ( entity.getVdbStatus() == RestMetadataVdbStatusVdb.NO_VDB_STATUS ) {
			out.nullValue();
		} else {
			BUILDER.toJson( entity.getVdbStatus(), RestMetadataVdbStatusVdb.class, out );
		}

		out.endObject();
	}

}
