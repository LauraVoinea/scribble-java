package org.scribble.ext.gt.core.job;

import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.lang.global.GProtocol;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.core.type.name.ModuleName;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.STypeFactory;
import org.scribble.core.type.session.Seq;
import org.scribble.core.visit.gather.RoleGatherer;
import org.scribble.core.visit.gather.STypeGatherer;
import org.scribble.ext.gt.core.model.global.GTSModelFactoryImpl;
import org.scribble.ext.gt.core.model.local.GTEModelFactoryImpl;
import org.scribble.util.ScribException;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class GTCore extends Core {

    public GTCore(ModuleName mainFullname, Map<CoreArgs, Boolean> args,
                  Set<GProtocol> imeds, STypeFactory tf) {
        super(mainFullname, args, imeds, tf);
    }

    @Override
    protected ModelFactory newModelFactory() {
        return new ModelFactory(GTEModelFactoryImpl::new,
                GTSModelFactoryImpl::new);
    }

    // TODO refactor
    class GTRoleGatherer<K extends ProtoKind, B extends Seq<K, B>>
            extends RoleGatherer<K, B> {

    }

    @Override
    protected void runGlobalSyntaxWfPasses() throws ScribException {
        verbosePrintPass(
                "Checking for unused role decls on all inlined globals...");
        for (ProtoName<Global> fullname : this.context.getParsedFullnames()) {
            // CHECKME: relegate to "warning" ? -- some downsteam operations may depend on this though (e.g., graph building?)
            System.out.println("11111111111: " + this.context.getInlined(fullname));
            System.out.println("11111111111: " + this.context.getInlined(fullname).def);
            Set<Role> used = this.context.getInlined(fullname).def

                    //.acceptNoThrow(new RoleGatherer<>())
                    .acceptNoThrow(new RoleGatherer<>())

                    .collect(Collectors.toSet());
            Set<Role> unused = this.context.getIntermediate(fullname).roles
                    // imeds have original role decls (inlined's are pruned)
                    .stream().filter(x -> !used.contains(x)).collect(Collectors.toSet());
            if (!unused.isEmpty()) {
                throw new ScribException(
                        "Unused roles in " + fullname + ": " + unused);
            }
        }

        verbosePrintPass("Checking role enabling on all inlined globals...");
        for (ProtoName<Global> fullname : this.context.getParsedFullnames()) {
            GProtocol unf = this.context.getOnceUnfolded(fullname);
            if (unf.isAux()) {
                continue;
            }
            unf.checkRoleEnabling(this);
            //e.g., C->D captured under an A->B choice after unfolding, cf. bad.wfchoice.enabling.twoparty.Test01b;
            // TODO: get unfolded from Context
        }

        verbosePrintPass(
                "Checking consistent external choice subjects on all inlined globals...");
        for (ProtoName<Global> fullname : this.context.getParsedFullnames()) {
            GProtocol inlined = this.context.getInlined(fullname);
            if (inlined.isAux()) {
                continue;
            }
            inlined.checkExtChoiceConsistency(this);
        }

        verbosePrintPass(
                "Checking connectedness on all inlined globals...");
        for (ProtoName<Global> fullname : this.context.getParsedFullnames()) {
            GProtocol unf = this.context.getOnceUnfolded(fullname);
            if (unf.isAux()) {
                continue;
            }
            unf.checkConnectedness(this, !unf.isExplicit());
            //e.g., rec X { connect A to B; continue X; }
        }
    }
}
