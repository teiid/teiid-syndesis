package org.komodo.rest.relational.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;

import org.junit.Test;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus;
import org.komodo.rest.relational.response.metadata.RestMetadataConnectionStatus.EntityState;

public final class MetadataConnectionStatusSerializerTest {

    private interface ActiveActive {

        String connectionName = "activeactive";
        int numErrors = 0;
        String schemaModelName = "activeactivebtlconnschemamodel";
        EntityState schemaState = EntityState.ACTIVE;
        String schemaVdbName = "activeactivebtlconnschemavdb";
        String vdbName = "activeactivebtlconn";
        EntityState vdbState = EntityState.ACTIVE;
        
        String json = 
            "{" + "\n\t"
            + "\"connectionName\": \"" + ActiveActive.connectionName + "\"," + "\n\t"
            + "\"vdbState\": \"" + ActiveActive.vdbState.toString() + "\"," + "\n\t"
            + "\"vdbName\": \"" + ActiveActive.vdbName + "\"," + "\n\t"
            + "\"schemaState\": \"" + ActiveActive.schemaState.toString() + "\"," + "\n\t"
            + "\"schemaVdbName\": \"" + ActiveActive.schemaVdbName + "\"," + "\n\t"
            + "\"schemaModelName\": \"" + ActiveActive.schemaModelName + "\"," + "\n\t"
            + "\"errors\": []" + "\n"
            + "}";
        
    }

    private interface ActiveMissing {

        String connectionName = "activemissing";
        int numErrors = 0;
        EntityState schemaState = EntityState.MISSING;
        String vdbName = "activemissingbtlconn";
        EntityState vdbState = EntityState.ACTIVE;
        
        String json = 
            "{" + "\n\t"
            + "\"connectionName\": \"" + ActiveMissing.connectionName + "\"," + "\n\t"
            + "\"vdbState\": \"" + ActiveMissing.vdbState.toString() + "\"," + "\n\t"
            + "\"vdbName\": \"" + ActiveMissing.vdbName + "\"," + "\n\t"
            + "\"schemaState\": \"" + ActiveMissing.schemaState.toString() + "\"," + "\n\t"
            + "\"errors\": []" + "\n"
            + "}";
        
    }

    private interface FailedMissing {
        
        String connectionName = "failedmissing";
        int numErrors = 2;
        EntityState schemaState = EntityState.MISSING;
        String vdbName = "failedmissingbtlconn";
        EntityState vdbState = EntityState.FAILED;

        String json = 
            "{" + "\n\t"
            + "\"connectionName\": \"" + FailedMissing.connectionName + "\"," + "\n\t"
            + "\"vdbState\": \"" + FailedMissing.vdbState.toString() + "\"," + "\n\t"
            + "\"vdbName\": \"" + FailedMissing.vdbName + "\"," + "\n\t"
            + "\"schemaState\": \"" + FailedMissing.schemaState.toString() + "\"," + "\n\t"
            + "\"errors\": [ \"error one\", \"error two\" ]" + "\n"
            + "}";
    
    }

    private interface LoadingMissing {
        
        String connectionName = "loadingmissing";
        int numErrors = 0;
        EntityState schemaState = EntityState.MISSING;
        String vdbName = "loadingmissingbtlconn";
        EntityState vdbState = EntityState.LOADING;

        String json = 
            "{" + "\n\t"
            + "\"connectionName\": \"" + LoadingMissing.connectionName + "\"," + "\n\t"
            + "\"vdbState\": \"" + LoadingMissing.vdbState.toString() + "\"," + "\n\t"
            + "\"vdbName\": \"" + LoadingMissing.vdbName + "\"," + "\n\t"
            + "\"schemaState\": \"" + LoadingMissing.schemaState.toString() + "\"," + "\n\t"
            + "\"errors\": []" + "\n"
            + "}";
    
    }

    private interface MissingMissing {

        String connectionName = "missingmissing";
        int numErrors = 0;
        EntityState schemaState = EntityState.MISSING;
        EntityState vdbState = EntityState.MISSING;
        
        String json = 
            "{" + "\n\t"
            + "\"connectionName\": \"" + MissingMissing.connectionName + "\"," + "\n\t"
            + "\"vdbState\": \"" + MissingMissing.vdbState.toString() + "\"," + "\n\t"
            + "\"schemaState\": \"" + MissingMissing.schemaState.toString() + "\"," + "\n\t"
            + "\"errors\": []" + "\n"
            + "}";
        
    }

    private void assertRoundTrip( final RestMetadataConnectionStatus original ) {
        final String roundTripJson = KomodoJsonMarshaller.marshall( original );
        final RestMetadataConnectionStatus roundTrip
            = KomodoJsonMarshaller.unmarshall( roundTripJson, RestMetadataConnectionStatus.class );
        assertThat( roundTrip.getConnectionName(), is( original.getConnectionName() ) );
        assertThat( roundTrip.getErrors().size(), is( original.getErrors().size() ) );
        assertThat( roundTrip.getSchemaModelName(), is( original.getSchemaModelName() ) );
        assertThat( roundTrip.getSchemaState(), is( original.getSchemaState() ) );
        assertThat( roundTrip.getSchemaVdbName(), is( original.getSchemaVdbName() ) );
        assertThat( roundTrip.getServerVdbName(), is( original.getServerVdbName() ) );
        assertThat( roundTrip.getServerVdbState(), is( original.getServerVdbState() ) );
    }

    @Test
    public void shouldRoundTripActiveActive() {
        final RestMetadataConnectionStatus original
            = KomodoJsonMarshaller.unmarshall( ActiveActive.json, RestMetadataConnectionStatus.class );
        assertThat( original.getConnectionName(), is( ActiveActive.connectionName ) );
        assertThat( original.getErrors().size(), is( ActiveActive.numErrors ) );
        assertThat( original.getSchemaModelName(), is( ActiveActive.schemaModelName ) );
        assertThat( original.getSchemaState(), is( ActiveActive.schemaState ) );
        assertThat( original.getSchemaVdbName(), is( ActiveActive.schemaVdbName ) );
        assertThat( original.getServerVdbName(), is( ActiveActive.vdbName ) );
        assertThat( original.getServerVdbState(), is( ActiveActive.vdbState ) );
        assertRoundTrip( original );
    }

    @Test
    public void shouldRoundTripActiveMissing() {
        final RestMetadataConnectionStatus original
            = KomodoJsonMarshaller.unmarshall( ActiveMissing.json, RestMetadataConnectionStatus.class );
        assertThat( original.getConnectionName(), is( ActiveMissing.connectionName ) );
        assertThat( original.getErrors().size(), is( ActiveMissing.numErrors ) );
        assertThat( original.getSchemaModelName(), is( nullValue() ) );
        assertThat( original.getSchemaState(), is( ActiveMissing.schemaState ) );
        assertThat( original.getSchemaVdbName(), is( nullValue() ) );
        assertThat( original.getServerVdbName(), is( ActiveMissing.vdbName ) );
        assertThat( original.getServerVdbState(), is( ActiveMissing.vdbState ) );
        assertRoundTrip( original );
    }

    @Test
    public void shouldRoundTripFailedMissing() {
        final RestMetadataConnectionStatus original
            = KomodoJsonMarshaller.unmarshall( FailedMissing.json, RestMetadataConnectionStatus.class );
        assertThat( original.getConnectionName(), is( FailedMissing.connectionName ) );
        assertThat( original.getErrors().size(), is( FailedMissing.numErrors ) );
        assertThat( original.getSchemaModelName(), is( nullValue() ) );
        assertThat( original.getSchemaState(), is( FailedMissing.schemaState ) );
        assertThat( original.getSchemaVdbName(), is( nullValue() ) );
        assertThat( original.getServerVdbName(), is( FailedMissing.vdbName ) );
        assertThat( original.getServerVdbState(), is( FailedMissing.vdbState ) );
        assertRoundTrip( original );
    }

    @Test
    public void shouldRoundTripLoadingMissing() {
        final RestMetadataConnectionStatus original
            = KomodoJsonMarshaller.unmarshall( LoadingMissing.json, RestMetadataConnectionStatus.class );
        assertThat( original.getConnectionName(), is( LoadingMissing.connectionName ) );
        assertThat( original.getErrors().size(), is( LoadingMissing.numErrors ) );
        assertThat( original.getSchemaModelName(), is( nullValue() ) );
        assertThat( original.getSchemaState(), is( LoadingMissing.schemaState ) );
        assertThat( original.getSchemaVdbName(), is( nullValue() ) );
        assertThat( original.getServerVdbName(), is( LoadingMissing.vdbName ) );
        assertThat( original.getServerVdbState(), is( LoadingMissing.vdbState ) );
        assertRoundTrip( original );
    }

    @Test
    public void shouldRoundTripMissingMissing() {
        final RestMetadataConnectionStatus original
            = KomodoJsonMarshaller.unmarshall( MissingMissing.json, RestMetadataConnectionStatus.class );
        assertThat( original.getConnectionName(), is( MissingMissing.connectionName ) );
        assertThat( original.getErrors().size(), is( MissingMissing.numErrors ) );
        assertThat( original.getSchemaModelName(), is( nullValue() ) );
        assertThat( original.getSchemaState(), is( MissingMissing.schemaState ) );
        assertThat( original.getSchemaVdbName(), is( nullValue() ) );
        assertThat( original.getServerVdbName(), is( nullValue() ) );
        assertThat( original.getServerVdbState(), is( MissingMissing.vdbState ) );
        assertRoundTrip( original );
    }

}
